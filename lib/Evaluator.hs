module Evaluator where 
    
import Types 
import Parser 



evalBExpr :: Expression -> Bool
evalBExpr (Constant t)                                    = t
evalBExpr (Product (Binary f) (Constant t) (Constant t')) = f (BoolChar t) (BoolChar t')
evalBExpr (One (Unary f) (Constant t))                    = f $ BoolChar t
evalBExpr (Product (Binary f) e (Constant t))             = f (BoolChar $ evalBExpr e) (BoolChar t)
evalBExpr (Product (Binary f) (Constant t) e)             = f (BoolChar t) (BoolChar $ evalBExpr e)
evalBExpr (Product (Binary f) e e')                       = f (BoolChar $ evalBExpr e) (BoolChar $ evalBExpr e')
evalBExpr (One (Unary f) e )                              = f (BoolChar $ evalBExpr e)