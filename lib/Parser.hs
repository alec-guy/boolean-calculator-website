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

parseConstant :: Parser Types.Expression
parseConstant = do 
    spaceParser
    eitherTrueOrFalse <- myLexemeWrapper $ Combinators.choice [MegaChar.char 'T', MegaChar.char' 'F']
    return $ Types.Constant $ (eitherTrueOrFalse == 'T')

parseTerm :: Parser Types.Expression 
parseTerm = do 
    spaceParser
    myLexemeWrapper (
     Combinators.choice
     [ parens parseExpression  -- Try parsing parenthesized expressions first
     , parseConstant          -- Then try parsing constants
     ])

parseExpression :: Parser Types.Expression
parseExpression = do 
    myLexemeWrapper $ Expr.makeExprParser parseTerm table 
    where   table = [ [prefix ["~"] (Types.One Types.norChar)
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

prefix :: [String] -> (Types.Expression -> Types.Expression) -> Expr.Operator Parser Types.Expression 
prefix l f = Expr.Prefix (f <$ (Combinators.choice (mySymbolParser <$> l)))
binary0 :: [String] -> (Types.Expression -> Types.Expression -> Types.Expression) -> Expr.Operator Parser Types.Expression 
binary0 l f = Expr.InfixR $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
binary1 :: [String] -> (Types.Expression -> Types.Expression -> Types.Expression) -> Expr.Operator Parser Types.Expression 
binary1 l f = Expr.InfixL $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
binary2 :: [String] -> (Types.Expression -> Types.Expression -> Types.Expression) -> Expr.Operator Parser Types.Expression 
binary2 l f = Expr.InfixN $ f <$ (Combinators.choice $ (mySymbolParser <$> l))
 
---------------------
---- Networking portion 

parseHTTP :: ByteParser Types.HTTP
parseHTTP = do
    statusLine0 <- parseStatusLine
    headers0    <- parseHeaders 
    body0       <- Combinators.many Mega.anySingle
    return $ Types.HTTP 
           {Types.statusLine = statusLine0
           ,Types.headers = headers0 
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
                               colon            <-  MegaByte.string ":"
                               let key = BS.concat [wordsBeforeColon, colon]
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
      httpVersion0    <- parseHttpVersion
      MegaByte.hspace
      statusCode0     <- parseStatusCode
      MegaByte.hspace
      reasonPhrase0   <- parseReasonPhrase
      parseCarriageReturn
      return $ Types.StatusLine
             {
                Types.httpVersion   = httpVersion0
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


