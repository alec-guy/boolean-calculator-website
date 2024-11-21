module Evaluator where 
    
import Types 
import Parser 



evalBExpr :: (Expression Bool Bool) -> Bool
evalBExpr (Constant t)                                    = t
evalBExpr (One (Unary f) e )                              = f ( evalBExpr e)
evalBExpr (Product (Binary f) e e')                       = f (evalBExpr e) ( evalBExpr e')


