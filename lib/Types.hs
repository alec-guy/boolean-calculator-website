module Types where 

newtype BoolChar = BoolChar Bool deriving (Eq, Show)

data BooleanOperation = Binary (BoolChar -> BoolChar -> Bool)
                      | Unary  (BoolChar -> Bool)
      

makeBinary :: (Bool -> Bool -> Bool) -> BoolChar -> BoolChar -> Bool
makeBinary f (BoolChar b) (BoolChar b') = f b b'
makeUnary :: (Bool -> Bool) -> BoolChar -> Bool 
makeUnary f (BoolChar b) = f b


data Expression = Constant Bool
                | Product BooleanOperation Expression Expression 
                | One BooleanOperation Expression


andChar = Binary $ makeBinary (&&)
orChar = Binary $ makeBinary (||)
ifThenChar = Binary $ makeBinary ifThen
   where ifThen = \b b1 -> not $ b && (not b1)
iffChar = Binary $ makeBinary iff
   where iff = \b b2 -> b == b2
xOrChar = Binary $ makeBinary xor 
   where xor = \b b1 -> b /= b1
nandChar = Binary $ makeBinary nand 
   where nand = \b b1 -> not (b && b1)
norChar = Binary $ makeBinary nor 
   where nor = \b b1 -> (not b) && (not b1)
notChar = Unary $ makeUnary not 
idChar = Unary $ makeUnary id






