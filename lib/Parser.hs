{-# LANGUAGE OverloadedStrings #-}


module Parser where 

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaChar
import qualified Text.Megaparsec.Char.Lexer as MegaLexer
import qualified Text.Megaparsec.Byte as MegaByte
import qualified Text.Megaparsec.Byte.Lexer as MegaByteLexer
import qualified Types as Types 
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Void 
import qualified Control.Monad as CM 
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word
import Control.Applicative ((<*))




type Parser = Mega.Parsec Void String


type MyState = Mega.State String Void 

type ByteParser = Mega.Parsec Void BS.ByteString 


spaceParser :: Parser ()
spaceParser = MegaLexer.space MegaChar.space1 Combinators.empty Combinators.empty 

-- Parser a -> Parser a
myLexemeWrapper = MegaLexer.lexeme spaceParser 

mySymbolParser :: String -> Parser String 
mySymbolParser = MegaLexer.symbol spaceParser 

parens :: Parser a -> Parser a
parens = Combinators.between (mySymbolParser "(") (mySymbolParser ")")

parseConstant :: Parser (Types.Expression Bool Bool) 
parseConstant = do 
    spaceParser
    eitherTrueOrFalse <- myLexemeWrapper $ Combinators.choice [MegaChar.char 'T', MegaChar.char' 'F']
    return $ Types.Constant $ (eitherTrueOrFalse == 'T')

parseTerm :: Parser (Types.Expression Bool Bool)
parseTerm = do 
    spaceParser
    myLexemeWrapper (
     Combinators.choice
     [ parens parseExpression  -- Try parsing parenthesized expressions first
     , parseConstant          -- Then try parsing constants
     ])

parseExpression :: Parser (Types.Expression Bool Bool)
parseExpression = do 
    expr <- (myLexemeWrapper $ Expr.makeExprParser parseTerm table)
    return expr
    where   table = [ [prefix ["~"] (Types.One Types.notChar)
                    ,prefix ["+"] (Types.One Types.idChar)
                    ]
                  , [binary0 ["&&"] (Types.Product Types.andChar)] 
                  , [binary1 ["||"] (Types.Product Types.orChar)
                    ,binary1 ["<->"] (Types.Product Types.iffChar)
                    ,binary1 ["->"] (Types.Product Types.ifThenChar)
                    ]
                  , 
                    [binary2 ["nand"] (Types.Product Types.nandChar)
                    ,binary2 ["nor"] (Types.Product Types.norChar)
                    ,binary2 ["xor"] (Types.Product Types.xOrChar)
                    ]
                  ]

prefix :: [String] -> ((Types.Expression Bool Bool) -> Types.Expression Bool Bool) 
                   -> Expr.Operator Parser (Types.Expression Bool Bool) 
prefix l f = Expr.Prefix (f <$ (Combinators.choice (mySymbolParser <$> l)))
binary0 :: [String] -> ((Types.Expression Bool Bool) -> (Types.Expression Bool Bool) -> (Types.Expression Bool Bool)) 
                    -> Expr.Operator Parser (Types.Expression Bool Bool)
binary0 l f = Expr.InfixR $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
binary1 :: [String] -> ((Types.Expression Bool Bool) -> (Types.Expression Bool Bool) -> (Types.Expression Bool Bool)) 
                    -> Expr.Operator Parser (Types.Expression Bool Bool)
binary1 l f = Expr.InfixL $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
binary2 :: [String] -> ((Types.Expression Bool Bool) -> (Types.Expression Bool Bool) -> (Types.Expression Bool Bool))
                    -> Expr.Operator Parser (Types.Expression Bool Bool)
binary2 l f = Expr.InfixN $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
 
---------------------
---- Networking portion 

parseHTTPResponse :: ByteParser Types.HTTPResponse
parseHTTPResponse = do
    statusLine0 <- parseStatusLine
    headersHR0  <- parseHeaders
    body0       <- Combinators.many Mega.anySingle
    return $ Types.HTTPResponse 
           {Types.statusLine = statusLine0
           ,Types.headersHR = headersHR0 
           ,Types.body = BS.pack body0
           }


  
parseHeaders :: ByteParser Types.Headers
parseHeaders = do
    keys  <- Combinators.manyTill (Mega.label "parsekeyandvalue" parseKeyAndValue) MegaByte.crlf 
    return $ Types.Headers { Types.pairs = Map.fromList keys}
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

parseStatusLine :: ByteParser Types.StatusLine
parseStatusLine = do
      versionHR0    <- parseHttpVersion
      MegaByte.hspace
      statusCode0     <- parseStatusCode
      MegaByte.hspace
      reasonPhrase0   <- parseReasonPhrase
      parseCarriageReturn
      return $ Types.StatusLine
             {
                Types.versionHR   = versionHR0
             ,  Types.statusCode    = statusCode0
             ,  Types.reasonPhrase  = reasonPhrase0
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
parseToken :: ByteParser Types.Token 
parseToken = do 
        let acmeChallenge = (MegaByte.string "acme-challenge") >> Mega.anySingle
        _      <- Combinators.skipManyTill Mega.anySingle acmeChallenge 
        _      <- acmeChallenge
        token  <- Combinators.manyTill Mega.anySingle (MegaByte.string " ")
        return $ Types.Token $ BS.pack token
  -- FOR ACME CHALLENGE
      
parseHTTPRequest :: ByteParser Types.HTTPRequest
parseHTTPRequest = do 
           method0    <- parseMethod 
           path0      <- parsePath
           version0   <- parseVersionResponse
           headers0   <- parseHeaders
           maybeBody0 <- parseMaybeBody
           return $ Types.HTTPRequest 
                  {Types.method    = method0
                  ,Types.path      = path0
                  ,Types.version  = version0 
                  ,Types.headers   = headers0
                  ,Types.maybeBody = maybeBody0
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

   
     