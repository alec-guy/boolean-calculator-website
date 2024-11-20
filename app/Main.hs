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



stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral . ord)

main :: IO ()
main = do
  putStrLn "Hello Haskell!"
  i <- getLine 
  case parse (Parser.parseExpression <* eof)  "" i  of 
    (Left e) -> putStrLn $ errorBundlePretty e 
    (Right expr) -> do 
                  putStrLn "Successful parse"
                  putStrLn $ show $ Evaluator.evalBExpr expr
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Nothing) (Just "80")
  let socketAddress = NS.addrAddress addr
  port80      <- NS.openSocket addr
  NS.bind port80 socketAddress 
  NS.listen port80 5
  putStrLn "Server listening on port 80"
  CM.forever $ do 
    (conn, clientAddr) <- NS.accept port80 
    putStrLn $ "Connection accepted from: " ++ show clientAddr 
    msg <- NSB.recv port80 4096 
    response <- handleAcmeChallenge acmeChallenge msg 
    NSB.sendAllTo port80 response socketAddress 


acmeChallenge :: String 
acmeChallenge = "/.well-known/acme-challenge/CpbCfjbf5HyeUSb1EvXiocPKvPhQ2PEd1TVbKdB4UX4.Q7e7KzdLGANIhPoShOTCxcjlvMsKQU-PMeUCEBCSSuk"

handleAcmeChallenge :: FilePath -> BS.ByteString -> IO BS.ByteString 
handleAcmeChallenge a _ = do 
          b <- BS.readFile acmeChallenge
          let header1 = "Content-Type: text/plain\r\n"
              fileSize = BS.pack $ stringToWord8 $ show $ BS.length b
              header2 = "Content-Length: " <> fileSize <> "\r\n\r\n"
              response = "HTTP/1.1 " <> "200 OK\r\n" <> header1 <> header2 <> b
          return response 

pathToHTML :: String 
pathToHTML = "/.src/index.html"

handleClient :: NS.Socket -> IO ()
handleClient conn = do 
  maybeCertificateStore <- readCertificateStore "C:/Users/alecb/certsTwo/cacert.pem"
  maybeCredential       <- TLS.credentialLoadX509 "C:/Users/alecb/logicCalcPrivateKey/public.pem" "C:/Users/alecb/logicCalcPrivateKey/private.pem"
  store                 <- (case maybeCertificateStore of
                             Nothing     -> error "could not find certificate store"
                             Just (store) ->  return store)
  credential            <- (case maybeCredential of 
                             Left s -> error s 
                             Right c -> return c)
  let 
      myBackend      = Types.MyBackend {Types.mySockey = conn}
      newShared      = (TLS.serverShared TLS.defaultParamsServer) {TLS.sharedCAStore = store}
      newShared'     = newShared {TLS.sharedCredentials = TLS.Credentials [credential]}  
      myParamsServer = TLS.defaultParamsServer {TLS.serverShared = newShared'}
  context <- TLS.contextNew myBackend myParamsServer
  TLS.handshake context 
  msg <- TLS.recvData context 
  putStrLn "Msg received"
  case (parse Parser.parseHTTPRequest "" msg) of 
           Left e -> return () 
           Right httpReq -> do 
                             let version0 = Types.version httpReq 
                                 method0  = Types.version httpReq 
                                 path0    = Types.path httpReq 
                                 pathCond = (False , path0 == (BS.pack $ stringToWord8 pathToHTML))
                             case (version0, method0 ) of 
                              ("HTTP/1.1", "GET") -> if snd pathCond
                                                     then do 
                                                           response <- makeHTTPResponse version0 method0 path0 
                                                           TLS.sendData context (BS.fromStrict response) 
                                                     else putStrLn "lmao , what do you want me to do with this shii boi ? XD"
                              _                   -> do 
                                                      putStrLn "I am too lazy for anything else" 
  BS.putStr msg
  SIO.hFlush SIO.stdout
  TLS.sendData context "Hello, client! "
  TLS.bye context 
  NS.close conn

makeHTTPResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
makeHTTPResponse version0 method0 path = do 
    case path == (BS.pack $ stringToWord8 pathToHTML) of 
      True ->  do 
                body0 <- BS.readFile pathToHTML
                let fileSize = BS.pack $ stringToWord8 $ show $ BS.length body0
                    headers = "Content-Type: text/html; charset=UTF-8\r\n" <> "Content-Length: " <> fileSize <> "\r\n\r\n"
                return $ version0 <> headers <> body0
      False -> do 
                return BS.empty 
