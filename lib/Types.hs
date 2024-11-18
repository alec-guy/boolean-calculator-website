module Types where 

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS as TLS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

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




---------------------------------
---     NETWORKING PORTION -----------

data HTTP = HTTP
          { statusLine :: StatusLine
          , headers :: Headers
          , body :: BS.ByteString 
          }
          deriving (Show, Eq)

data StatusLine = StatusLine
                {   httpVersion :: BS.ByteString
                ,   statusCode :: Int
                ,   reasonPhrase :: BS.ByteString
                }
                deriving (Eq, Show)

data Headers = Headers{pairs :: (Map.Map BS.ByteString BS.ByteString)} deriving (Show, Eq)

data MyBackend = MyBackend {mySockey :: NS.Socket} deriving (Show, Eq)

instance TLS.HasBackend MyBackend where
    initializeBackend mb = return ()
    getBackend myb = TLS.Backend
                   {
                    TLS.backendFlush = return ()
                   ,TLS.backendClose = do 
                                    NS.close' $ mySockey myb
                                    putStrLn $ "Closed socket " ++ (show $ mySockey myb) ++ "..."

                   ,TLS.backendSend = \bytes -> do
                                 sent <- NSB.send (mySockey myb) bytes
                                 return ()
                   ,TLS.backendRecv = \size -> readUntil (mySockey myb) BS.empty size 
                   }
   
readUntil :: NS.Socket -> BS.ByteString -> Int -> IO BS.ByteString 
readUntil socky dataSoFar i = do 
    chunk <- NSB.recv socky i 
    case BS.null chunk of 
       True -> return dataSoFar 
       False -> do 
          let  x              = dataSoFar <> chunk
               remainingBytes = i - (BS.length x)
          case remainingBytes <= 0 of 
            True  -> return $ x 
            False -> readUntil socky x remainingBytes
