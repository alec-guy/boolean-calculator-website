module Evaluator where 
    
import Types 
import Parser 



evalBExpr :: (Expression Bool Bool) -> Bool
evalBExpr (Constant t)                                    = t
evalBExpr (Product (Binary f) (Constant t) (Constant t')) = f t t'
evalBExpr (One (Unary f) (Constant t))                    = f t
evalBExpr (Product (Binary f) e (Constant t))             = f ( evalBExpr e) t
evalBExpr (Product (Binary f) (Constant t) e)             = f t (evalBExpr e)
evalBExpr (Product (Binary f) e e')                       = f (evalBExpr e) ( evalBExpr e')
evalBExpr (One (Unary f) e )                              = f ( evalBExpr e)