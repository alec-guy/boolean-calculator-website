{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified System.Exit 
import qualified Types as Types 
import qualified System.IO as SIO
import qualified Control.Exception as CE
import qualified Parser as Parser 
import qualified Evaluator as Evaluator
import Text.Megaparsec (parse, eof, parseMaybe)
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
import qualified Data.Map.Strict as Map
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Aeson
import GHC.Generics


data ParseErrorOrEvaluation = ParseErrorOrEvaluation
                            {parseError :: String 
                            ,evaluation :: String 
                            } deriving (Generic,Show)
instance ToJSON ParseErrorOrEvaluation where 
   -- No need to provide a toJSON implementation.. 
   toEncoding = genericToEncoding defaultOptions 
  
instance FromJSON ParseErrorOrEvaluation 
      -- no need to provide a parseJSON implementation. 



main :: IO ()
main = do
  _ <- Concurrent.forkIO $ safeServer httpServer 
  _ <- Concurrent.forkIO $ safeServer httpsServer 
  putStrLn "Servers are running.. Press Ctrl+C to stop."
  CM.forever $ Concurrent.threadDelay maxBound


safeServer :: IO () -> IO ()
safeServer server = server `CE.catch` \e -> do 
     putStrLn $ "Server encountered an error: " ++ show (e :: CE.SomeException)
     safeServer server -- Restatrt the server

hostName :: BS.ByteString 
hostName = "localhost:8002"

httpServer :: IO () 
httpServer = do 
  putStrLn "HTTP server is running..."
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Just $ "localhost") (Just "8002")
  let socketAddress = NS.addrAddress addr
  port      <- NS.openSocket addr
  NS.setSocketOption port NS.ReuseAddr 1
  NS.bind port socketAddress 
  NS.listen port 5
  putStrLn "Server listening on port 8002"
  CM.forever $ do 
    (conn, clientAddr) <- NS.accept port 
    putStrLn $ "HTTP Connection accepted from: " ++ show clientAddr 
    msg      <- recvHTTP conn BS.empty 
    response <- handleMsg msg 
    NSB.sendAll conn response 
    putStrLn "Sent it to connection. " 
    NS.close conn
  
recvHTTP :: NS.Socket -> BS.ByteString -> IO BS.ByteString 
recvHTTP s oldBytes = do 
      newBytes <- NSB.recv s 4096
      let oldPlusNew = oldBytes <> newBytes  
      case BS.isInfixOf "\r\n\r\n" oldPlusNew of 
        True -> return oldPlusNew
        False -> recvHTTP s oldPlusNew 

  
httpsServer :: IO ()
httpsServer = do 
  putStrLn "HTTPS server is running..."
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Nothing) (Just "8006")
  let socketAddress = NS.addrAddress addr
  port      <- NS.openSocket addr
  NS.setSocketOption port NS.ReuseAddr 1
  NS.bind port socketAddress 
  NS.listen port 5
  putStrLn "Server listening on port 8006"
  CM.forever $ do 
    (conn, clientAddr) <- NS.accept port 
    putStrLn $ "HTTPS Connection accepted from: " ++ show clientAddr 
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
  case parse (Parser.parseHTTPRequest <* eof) "" msg of
    Left e -> do
      putStrLn "Error parsing message..."
      putStrLn $ errorBundlePretty e
      putStr "Message: "
      SIO.hFlush SIO.stdout
      BS.putStr msg
      SIO.hFlush SIO.stdout
      return BS.empty

    Right httpReq -> do
      let version0   = Types.version httpReq 
          method0    = Types.method httpReq
          path0      = Types.path httpReq
          maybeHost  = Map.lookup "Host" $ Types.pairs $ Types.headers httpReq
      putStr "Message: "
      SIO.hFlush SIO.stdout
      putStrLn $ show $ httpReq 
      SIO.hFlush SIO.stdout

      case (version0, method0, BS.isInfixOf hostName $ fromJust maybeHost) of
        ("HTTP/1.1", "GET", True) -> do 
                    putStrLn "Making httpResponse, found version and method"
                    response <- makeHTTPResponse version0 method0 path0
                    return response

        (_, _ , False) -> do 
                    putStrLn "Making httpResponse, did not find host name"
                    return BS.empty 

makeHTTPResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
makeHTTPResponse version0 method0 path = do 
    let bsPath s = BS.pack $ stringToWord8 s
        maybeAcmeChallenge = parseMaybe Parser.parseToken path
    case path of 
      "/"   ->  do 
                 body0 <- BS.readFile pathToHTML 
                 putStrLn "Successfuly read HTML file and now going to send it."
                 let fileSize = BS.pack $ stringToWord8 $ show $ BS.length body0
                     headers = "Content-Type: text/html; charset=UTF-8\r\n" <> "Content-Length: " <> fileSize <> "\r\n\r\n"
                 return $ "HTTP/1.1" <>  " 200 OK\r\n" <> headers <> body0
      "/elmJson" -> return BS.empty 
      _          -> do 
                     case maybeAcmeChallenge of 
                      Nothing -> return BS.empty 
                      (Just token) -> do 
                                 let body0    = case token of 
                                                 (Types.Token b) -> b
                                     fileSize = BS.pack $ stringToWord8 $ show $ BS.length body0 
                                     headers =  "Content-Type: text; charset=UTF-8\r\n" <> "Content-Length: " <> fileSize <> "\r\n\r\n"
                                 return $ version0 <> " 200 OK \r\n" <> headers <> body0 


stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral . ord)

pathToHTML :: String 
pathToHTML = "C:\\Users\\alecb\\logic\\frontend\\index.html"