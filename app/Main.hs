{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Types as Types 
import qualified System.IO as SIO
import qualified Control.Exception as CE
import qualified Parser as Parser 
import qualified Evaluator as Evaluator
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Error 
import Control.Applicative ((<*))
import qualified Control.Monad as CM
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher
import Data.X509.CertificateStore 
import qualified Data.ByteString as BS
import Data.Word 
import qualified Data.List.NonEmpty as NE
import Data.Char (ord)
import qualified System.Exit 
import Control.Concurrent 



main :: IO ()
main = do
  httpServer
  httpsServer

httpServer :: IO () 
httpServer = do 
  putStrLn "HTTP server is running..."
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Nothing) (Just "80")
  let socketAddress = NS.addrAddress addr
  port80      <- NS.openSocket addr
  NS.bind port80 socketAddress 
  NS.listen port80 5
  putStrLn "Server listening on port 80"
  CM.forever $ do 
    (conn, clientAddr) <- NS.accept port80 
    putStrLn $ "Connection accepted from: " ++ show clientAddr 
    msg      <- Types.readUntil conn BS.empty 4096
    response <- handleMsg msg 
    NSB.sendAllTo port80 response socketAddress 

httpsServer :: IO ()
httpsServer = do 
  putStrLn "HTTPS server is running..."
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Nothing) (Just "443")
  let socketAddress = NS.addrAddress addr
  port443      <- NS.openSocket addr
  NS.bind port443 socketAddress 
  NS.listen port443 5
  putStrLn "Server listening on port 80"
  CM.forever $ do 
    (conn, clientAddr) <- NS.accept port443 
    putStrLn $ "Connection accepted from: " ++ show clientAddr 
    maybeCertificateStore <- readCertificateStore "C:/Users/alecb/certsTwo/cacert.pem"
    maybeCredential       <- TLS.credentialLoadX509 "C:/Users/alecb/logicCalcPrivateKey/public.pem" "C:/Users/alecb/logicCalcPrivateKey/private.pem"
    store                 <- (case maybeCertificateStore of
                              Nothing     -> error "could not find certificate store"
                              Just (store) ->  return store)
    credential            <- (case maybeCredential of 
                              Left s -> error s 
                              Right c -> return c)
    let myBackend      = Types.MyBackend {Types.mySockey = conn}
        newShared      = (TLS.serverShared TLS.defaultParamsServer) {TLS.sharedCAStore = store}
        newShared'     = newShared {TLS.sharedCredentials = TLS.Credentials [credential]}  
        myParamsServer = TLS.defaultParamsServer {TLS.serverShared = newShared'}
    context <- TLS.contextNew myBackend myParamsServer
    TLS.handshake context 
    msg      <- TLS.recvData context 
    response <- handleMsg msg 
    TLS.sendData context (BS.fromStrict response) 
    TLS.bye context 
    NS.close conn

--------------------------------------------------------------------------------------
----        HELPER        STUFF BECAUSE THE GUYS AT THE TOP ARE NOT STRONG ENOUGH I GUESS  -----------------

handleMsg :: BS.ByteString -> IO BS.ByteString
handleMsg msg = do 
  case (parse Parser.parseHTTPRequest "" msg) of 
    Left e -> do  
               putStrLn "Error parsing message..."
               putStrLn $ errorBundlePretty e 
               putStr "Message: "
               SIO.hFlush SIO.stdout 
               BS.putStr msg 
               SIO.hFlush SIO.stdout
               return BS.empty  
    Right httpReq -> do 
                      let version0 = Types.version httpReq 
                          method0  = Types.version httpReq 
                          path0    = Types.path httpReq 
                      case (version0, method0 ) of 
                        ("HTTP/1.1", "GET") -> do 
                                      response <- makeHTTPResponse version0 method0 path0 
                                      return response 
                        _                   -> return BS.empty 

makeHTTPResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
makeHTTPResponse version0 method0 path = do 
    case path == (BS.pack $ stringToWord8 pathToHTML) of 
      True ->  do 
                body0 <- BS.readFile pathToHTML
                let fileSize = BS.pack $ stringToWord8 $ show $ BS.length body0
                    headers = "Content-Type: text/html; charset=UTF-8\r\n" <> "Content-Length: " <> fileSize <> "\r\n\r\n"
                return $ version0 <> headers <> body0
      False -> return BS.empty 

stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral . ord)

pathToHTML :: String 
pathToHTML = "/.src/index.html"