{-# LANGUAGE OverloadedStrings #-}


module HTTPParser where 

import HTTPTypes 
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte as MegaByte
import qualified Text.Megaparsec.Byte.Lexer as MegaByteLexer
import qualified Control.Monad.Combinators as Combinators
import Data.Void 
import qualified Control.Monad as CM 
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word
import Control.Applicative ((<*))


type ByteParser = Mega.Parsec Void BS.ByteString 


parseHTTPResponse :: ByteParser HTTPResponse
parseHTTPResponse = do
    statusLine0 <- parseStatusLine
    headersHR0  <- parseHeaders
    body0       <- Combinators.many Mega.anySingle
    return $ HTTPResponse 
           {statusLine = statusLine0
           ,headersHR = headersHR0 
           ,body = BS.pack body0
           }


  
parseHeaders :: ByteParser Headers
parseHeaders = do
    keys  <- Combinators.manyTill (Mega.label "parsekeyandvalue" parseKeyAndValue) MegaByte.crlf 
    return $ Headers { pairs = Map.fromList keys}
    where parseKeyAndValue :: ByteParser (BS.ByteString, BS.ByteString)
          parseKeyAndValue = do
                    key   <- parseKey
                    value <- (Mega.try (parseTypicalValue)) Mega.<|> (BS.concat <$> pMultilinedValue)
                    return $ (key, value)
                    where parseKey :: ByteParser BS.ByteString
                          parseKey = do
                               wordsBeforeColon <-  Mega.takeWhileP (Just "key") (\c -> c /= (fromIntegral . ord $ ':'))
                               _            <-  MegaByte.string ":"
                               let key = wordsBeforeColon
                               return $ key
          parseTypicalValue :: ByteParser BS.ByteString
          parseTypicalValue = do
                            value <- Mega.takeWhileP (Just "value") (\c -> c /=  (fromIntegral . ord $ '\r'))
                            MegaByte.crlf 
                            return value
          pMultilinedValue :: ByteParser [BS.ByteString]
          pMultilinedValue = pUniqueValue `monadAppend` ((Mega.try pMultilinedValue) >> pValueOrEnd)
                        where pUniqueValue = do value <- Mega.takeWhileP (Just "value") (\c -> c /=  (fromIntegral . ord $ '\r'))
                                                _     <- MegaByte.crlf
                                                _     <- Combinators.eitherP MegaByte.hspace1 MegaByte.tab
                                                return $ value
                              pValueOrEnd = do
                                            result <- Combinators.eitherP parseTypicalValue MegaByte.crlf
                                            case result of
                                                Left value' -> return [value']
                                                Right _     -> return []
                              monadAppend :: Monad m => m a -> m [a] -> m [a]
                              monadAppend = CM.liftM2 (:)

parseStatusLine :: ByteParser StatusLine
parseStatusLine = do
      versionHR0    <- parseHttpVersion
      MegaByte.hspace
      statusCode0     <- parseStatusCode
      MegaByte.hspace
      reasonPhrase0   <- parseReasonPhrase
      parseCarriageReturn
      return $ StatusLine
             {
                versionHR   = versionHR0
             ,  statusCode    = statusCode0
             ,  reasonPhrase  = reasonPhrase0
             }
      where parseHttpVersion :: ByteParser BS.ByteString
            parseHttpVersion = 
              ((Mega.try (MegaByte.string "HTTP/1.1")) Mega.<|> (Mega.try $ MegaByte.string "HTTP/1.0"))  Mega.<|> (MegaByte.string "HTTP/2.0")
            parseStatusCode :: ByteParser Int
            parseStatusCode = MegaByteLexer.decimal
            parseReasonPhrase :: ByteParser BS.ByteString
            parseReasonPhrase = Mega.takeWhileP (Just "letter Of Reason") (\c -> c /= (fromIntegral . ord  $ '\CR'))
            parseCarriageReturn :: ByteParser BS.ByteString
            parseCarriageReturn = MegaByte.crlf


--- 
parseToken :: ByteParser Token 
parseToken = do 
        let acmeChallenge = (MegaByte.string "acme-challenge") >> Mega.anySingle
        _      <- Combinators.skipManyTill Mega.anySingle acmeChallenge 
        _      <- acmeChallenge
        token  <- Combinators.manyTill Mega.anySingle (MegaByte.string " ")
        return $ Token $ BS.pack token
  -- FOR ACME CHALLENGE
      
parseHTTPRequest :: ByteParser HTTPRequest
parseHTTPRequest = do 
           method0    <- parseMethod 
           path0      <- parsePath
           version0   <- parseVersionResponse
           headers0   <- parseHeaders
           maybeBody0 <- parseMaybeBody
           return $ HTTPRequest 
                  {method    = method0
                  ,path      = path0
                  ,version  = version0 
                  ,headers   = headers0
                  ,maybeBody = maybeBody0
                  }
           where parseMaybeBody = do 
                      e <- Combinators.eitherP (MegaByte.crlf >> MegaByte.crlf) (return ())
                      case e of 
                        Left _ -> return Nothing 
                        Right l -> do 
                                    rest <- Mega.takeRest 
                                    return $ Just $ rest
   
parseMethod :: ByteParser BS.ByteString 
parseMethod = do 
       Combinators.choice [MegaByte.string "GET"
                          ,MegaByte.string "DELETE"
                          ,MegaByte.string "HEAD"
                          ,MegaByte.string "POST"
                          ,MegaByte.string "PUT"
                          ,MegaByte.string "CONNECT"
                          ,MegaByte.string "TRACE"
                          ,MegaByte.string "PATCH"
                          ]

parsePath :: ByteParser BS.ByteString 
parsePath = do 
     MegaByte.hspace1 
     root <- MegaByte.string "/" :: ByteParser BS.ByteString
     path <- Combinators.manyTill Mega.anySingle (MegaByte.string " ") 
     return $ (root <> BS.pack path)
parseVersionResponse :: ByteParser BS.ByteString 
parseVersionResponse = do 
      version <- ((Mega.try (MegaByte.string "HTTP/1.1")) Mega.<|> (Mega.try $ MegaByte.string "HTTP/1.0"))  Mega.<|> (MegaByte.string "HTTP/2.0")
      MegaByte.crlf 
      return version

   
     