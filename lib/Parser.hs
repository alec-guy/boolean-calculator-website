module Parser where 

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaChar
import qualified Text.Megaparsec.Char.Lexer as MegaLexer
import qualified Types as Types 
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Void 



type Parser = Mega.Parsec Void String


type MyState = Mega.State String Void 

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
 



