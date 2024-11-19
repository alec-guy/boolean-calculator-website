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
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher
import Data.X509.CertificateStore 
import qualified Data.ByteString as BS
import Data.Word 
import qualified Data.List.NonEmpty as NE
import Data.Char (ord)



stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral . ord)

main :: IO ()
main = do
  putStrLn "Hello Haskell!"
  maybeCertificateStore <- readCertificateStore "C:/Users/alecb/certsTwo/cacert.pem"
  maybeCredential       <- TLS.credentialLoadX509 "C:/Users/alecb/logicCalcPrivateKey/public.pem" "C:/Users/alecb/logicCalcPrivateKey/private.pem"
  store                 <- (case maybeCertificateStore of
                             Nothing     -> error "could not find certificate store"
                             Just (store) ->  return store)
  credential            <- (case maybeCredential of 
                             Left s -> error s 
                             Right c -> return c)
  addr                  <-  NE.head <$> NS.getAddrInfo (Just NS.defaultHints) (Just "Nothing") (Just "3000")
  let socketAddress = NS.addrAddress addr
  port3000      <- NS.openSocket addr
  socketType    <- NS.getSocketType port3000
  let myBackend      = Types.MyBackend {Types.mySockey = port3000}
      newShared      = (TLS.serverShared TLS.defaultParamsServer) {TLS.sharedCAStore = store}
      newShared'     = newShared {TLS.sharedCredentials = TLS.Credentials [credential]}  
      myParamsServer = TLS.defaultParamsServer {TLS.serverShared = newShared'}
  NS.bind port3000 socketAddress 
  NS.listen port3000 5
  putStrLn "Server listening on port 3000"
  forever $ do 
    (conn, clientAddr) <- NS.accept sock 
  context <- TLS.contextNew myBackend myParamsServer
  putStrLn "Testing Expression parser."
  i <- getLine 
  case parse (Parser.parseExpression <* eof)  "" i  of 
    (Left e) -> putStrLn $ errorBundlePretty e 
    (Right expr) -> do 
                  putStrLn "Successful parse"
                  putStrLn $ show $ Evaluator.evalBExpr expr
  TLS.bye context


  
        

