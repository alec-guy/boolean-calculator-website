module Evaluator where 
    
import Types 
import Parser 



evalBExpr :: (Expression Bool Bool) -> Bool
evalBExpr (Constant t)                                = id t
evalBExpr (One Not e )                              = ~ ( evalBExpr e)
evalBExpr (Product And e e')                       =  (evalBExpr e) && ( evalBExpr e')
evalBExpr (Product Or e e')                        = 


