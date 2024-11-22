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


toGateAndOut :: Gate Bool -> (String, Bool) 
toGateAndOut (Wire b)    =   ("wire", b)
toGateAndOut (Not g)     =   ("not", evalGate (Not g))
toGateAndOut (And g g')  =   ("and", evalGate (And g g'))
toGateAndOut (Or g g')   =   ("or" , evalGate (Or g g'))
toGateAndOut (If g g')   =   ("if", evalGate (If g g'))
toGateAndOut (Iff g g')  =   ("iff", evalGate (Iff g g'))
toGateAndOut (Nand g g') =   ("nand", evalGate (Nand g g'))
toGateAndOut (Nor g g')  =   ("nor", evalGate (Nor g g'))
toGateAndOut (Xor g g')  =   ("xor" , evalGate (Xor g g'))
toGateAndOut (Id g)      =   ("id", evalGate (Id g))

collectGates :: Gate Bool -> [Gate Bool]
collectGates (Wire b)    = [Wire b]
collectGates (Not g)     = (Not g) : (collectGates g)
collectGates (And g g')  = (And g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Or g g')   = (Or g g') : ((collectGates g) ++ (collectGates g'))
collectGates (If g g')   = (If g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Iff g g')  = (Iff g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Nand g g') = (Nand g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Nor g g')  = (Nor g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Xor g g')  = (Xor g g') : ((collectGates g) ++ (collectGates g'))
collectGates (Id g)      = (Id g) : (collectGates g)

collectGatesAndOuts :: Gate Bool -> [(String, Bool)]
collectGatesAndOuts g = do 
       gate <- collectGates g 
       return $ toGateAndOut gate
          