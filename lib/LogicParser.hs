
module LogicParser where 

import LogicTypes 
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaChar
import qualified Text.Megaparsec.Char.Lexer as MegaLexer
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.Expr as Expr
import Data.Void 
import qualified Control.Monad as CM 

type Parser = Mega.Parsec Void String

spaceParser :: Parser ()
spaceParser = MegaLexer.space MegaChar.space1 Combinators.empty Combinators.empty 

-- Parser a -> Parser a
myLexemeWrapper = MegaLexer.lexeme spaceParser 

mySymbolParser :: String -> Parser String 
mySymbolParser = MegaLexer.symbol spaceParser 

parens :: Parser a -> Parser a
parens = Combinators.between (mySymbolParser "(") (mySymbolParser ")")

parseConstant :: Parser (Gate Bool)
parseConstant = do 
    spaceParser
    eitherTrueOrFalse <- myLexemeWrapper $ Combinators.choice $ 
                                             [MegaChar.char 'T'
                                             ,MegaChar.char' 'F'
                                             ,MegaChar.char '1'
                                             ,MegaChar.char '0']
    return $ Wire $ (eitherTrueOrFalse == 'T')

parseTerm :: Parser (Gate Bool) 
parseTerm = do 
    spaceParser
    myLexemeWrapper (
     Combinators.choice
     [ parens parseExpression  -- Try parsing parenthesized expressions first
     , parseConstant          -- Then try parsing constants
     ])

parseExpression :: Parser (Gate Bool)
parseExpression = do 
    expr <- (myLexemeWrapper $ Expr.makeExprParser parseTerm table)
    return expr
    where
        table = [[Expr.Postfix parseNot ,Expr.Postfix parseId] 
                ,[Expr.InfixR parseAnd]
                ,[Expr.InfixR parseOr
                 ,Expr.InfixR parseIf
                 ,Expr.InfixR parseIff
                 ]
                ,[Expr.InfixL parseXor 
                 ,Expr.InfixL parseNand
                 ,Expr.InfixL parseNor
                 ]
                ]

parseNot = do 
    CM.void  $ Combinators.choice 
        [mySymbolParser "~", mySymbolParser "\x00AC", mySymbolParser "!"] 
    return Not

parseId = do
    CM.void $ mySymbolParser "+"
    return Id

parseAnd = do
    CM.void $ Combinators.choice 
        [mySymbolParser "&", mySymbolParser "\x00b7", mySymbolParser "\x2227"]
    return And

parseOr = do
    CM.void $ Combinators.choice 
        [mySymbolParser "\x2228", mySymbolParser "\x002B", mySymbolParser "\x2225"]
    return Or

parseIf = do
    CM.void $ Combinators.choice 
        [mySymbolParser "\x21D2", mySymbolParser "\x2192", mySymbolParser "\x2283"]
    return If

parseIff = do
    CM.void $ Combinators.choice 
        [mySymbolParser "\x21D4", mySymbolParser "\x2192", mySymbolParser "\x2261"]
    return Iff

parseXor = do
    CM.void $ Combinators.choice 
        [mySymbolParser "\x2295", mySymbolParser "\x22BB", mySymbolParser "\x21AE", mySymbolParser "\x2262"]
    return Xor

parseNand = do
    CM.void $ mySymbolParser "\x22BC"
    return Nand

parseNor = do
    CM.void $  mySymbolParser "\x22BD"
    return Nor