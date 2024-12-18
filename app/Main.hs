{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as Concurrent
import HTTPParser 
import HTTPTypes 
import LogicParser 
import qualified System.IO as SIO
import qualified Control.Exception as CE
import qualified Evaluator as Evaluator
import Text.Megaparsec hiding (parseError)
import qualified Text.Megaparsec.Byte as MegaByte
import qualified Control.Monad.Combinators as Combinators
import Text.Megaparsec.Error 
import Control.Applicative ((<*), (*>))
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


data JsonR = JsonR
           {parseError :: String 
           ,evaluation :: String 
           ,gatesAndOuts :: [(String,Bool)]
           } deriving (Generic,Show)
instance ToJSON JsonR where 
   -- No need to provide a toJSON implementation.. 
   toEncoding = genericToEncoding defaultOptions 
  
instance FromJSON JsonR 
      -- no need to provide a parseJSON implementation. 

data ElmReq = ElmReq
            {
              booleanExpression :: String 
            } deriving (Generic, Show)
instance FromJSON ElmReq 


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
    msg <- NSB.recv conn 4096
    response <- handleMsg msg 
    NSB.sendAll conn response 
    putStrLn "Sent it to connection. " 
    NS.close conn

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
    let myBackend      = MyBackend {mySockey = conn}
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
  case parse (parseHTTPRequest <* eof) "" msg of
    Left e -> do
      putStrLn "Error parsing message..."
      putStrLn $ errorBundlePretty e
      putStr "Message: "
      SIO.hFlush SIO.stdout
      BS.putStr msg
      SIO.hFlush SIO.stdout
      return BS.empty

    Right httpReq -> do
      let version0   = version httpReq 
      putStr "HTTP request: "
      SIO.hFlush SIO.stdout
      putStrLn $ show $ httpReq 
      SIO.hFlush SIO.stdout

      case version0 of
        "HTTP/1.1" -> do 
                    putStrLn "Making httpResponse, found version and method"
                    response <- makeHTTPResponse httpReq 
                    return response

        _         -> do putStrLn "I am stupid" >> return BS.empty

makeHTTPResponse :: HTTPRequest -> IO BS.ByteString
makeHTTPResponse httpreq = 
      case (path httpreq) of 
       "/" -> do 
               html <- BS.readFile pathToHTML
               let contentLength = BS.length html 
               return $ 
                 (version httpreq) <>  
                 " 200 OK\r\n" <> 
                 "Content: text/html\r\nContent-Length: " <>
                 (BS.pack $ stringToWord8 $ show contentLength)  <>
                 "\r\n\r\n" <> 
                 html 
       "/upload" -> do 
                     let httpreqBody = case maybeBody httpreq of 
                                        Nothing  -> ""
                                        (Just b) -> case (decode (BS.fromStrict b) :: Maybe ElmReq) of 
                                                     Nothing -> "" 
                                                     (Just elmreq) -> booleanExpression elmreq 
                         jsonR = case parse parseExpression "" (httpreqBody) of 
                                  Left e -> JsonR {parseError  = errorBundlePretty e 
                                                  ,evaluation = "Nothing"
                                                  ,gatesAndOuts = []
                                                  }
                                  Right expr -> let evaluated = Evaluator.evalGate expr 
                                                in JsonR {parseError   = ""
                                                         ,evaluation   = show evaluated
                                                         ,gatesAndOuts = Evaluator.collectGatesAndOuts expr
                                                         }
                         encodedJson  = BS.toStrict $ encode jsonR 
                         contentLength = BS.length encodedJson
                         response      = (version httpreq) <>  
                                           " 200 OK\r\n" <> 
                                           "Content: application/json\r\nContent-Length: " <>
                                           (BS.pack $ stringToWord8 $ show contentLength)  <>
                                           "\r\n\r\n" <>
                                           encodedJson
                     
                     BS.putStr response 
                     SIO.hFlush SIO.stdout 
                     return $ response 
       "/images/rootBeerAvatar.png"  -> do 
                                          png <- BS.readFile "frontend/images/rootBeerAvatar.png"
                                          let contentLength = BS.length png 
                                          return $ 
                                            (version httpreq) <>
                                            " 200 OK\r\n" <>
                                            "Content: image/png\r\nConent-Lengh: " <> 
                                            (BS.pack $ stringToWord8 $ show contentLength) <>
                                            "\r\n\r\n" <>
                                            png
       "/images/jakeTheDog.jpg"  -> do 
                                        jpg <- BS.readFile "frontend/images/jakeTheDog.jpg"
                                        let contentLength = BS.length jpg 
                                        return $ 
                                          (version httpreq) <>
                                          " 200 OK\r\n" <>
                                          "Content: image/jpg\r\nConent-Lengh: " <> 
                                          (BS.pack $ stringToWord8 $ show contentLength) <>
                                          "\r\n\r\n" <>
                                          jpg
       "/images/bubblegum.png"  -> do 
                                    png <- BS.readFile "frontend/images/bubblegum.png"
                                    let contentLength = BS.length png 
                                    return $ 
                                        (version httpreq) <>
                                        " 200 OK\r\n" <>
                                        "Content: image/png\r\nConent-Lengh: " <> 
                                        (BS.pack $ stringToWord8 $ show contentLength) <>
                                        "\r\n\r\n" <>
                                        png
       "/images/fin.png"  -> do 
                              png <- BS.readFile "frontend/images/fin.png"
                              let contentLength = BS.length png 
                              return $ 
                                  (version httpreq) <>
                                  " 200 OK\r\n" <>
                                  "Content: image/png\r\nConent-Lengh: " <> 
                                  (BS.pack $ stringToWord8 $ show contentLength) <>
                                  "\r\n\r\n" <>
                                  png
       "/images/marceline.png"  -> do 
                                    png <- BS.readFile "frontend/images/marceline.png"
                                    let contentLength = BS.length png 
                                    return $ 
                                        (version httpreq) <>
                                        " 200 OK\r\n" <>
                                        "Content: image/png\r\nConent-Lengh: " <> 
                                        (BS.pack $ stringToWord8 $ show contentLength) <>
                                        "\r\n\r\n" <>
                                        png

       
       _         -> return $ 
                     (version httpreq) <> 
                     " 404 Not Found\r\n\r\n"

stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral . ord)

pathToHTML :: String 
pathToHTML = "C:\\Users\\alecb\\logic\\frontend\\index.html"

