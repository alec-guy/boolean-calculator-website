module Evaluator where 
    
import LogicTypes 

evalGate :: Gate Bool -> Bool
evalGate (Wire b)                          = id b
evalGate (Not g )                          = not ( evalGate g)
evalGate (And g g')                        =  (evalGate g) && ( evalGate g')
evalGate (Or g g')                         = (evalGate g) || ( evalGate g')
evalGate (If g g')                         =  not ((evalGate g) && (not (evalGate g')))
evalGate (Iff g g')                        = (evalGate g) == (evalGate g') 
evalGate (Nand g g')                       = not ((evalGate g) && (evalGate g'))
evalGate (Nor g g')                        = not ((evalGate g) || (evalGate g'))
evalGate (Xor g g')                        = (evalGate g) /= (evalGate g')
evalGate (Id g)                            = (evalGate g)


