
module Except (Excepted
              ,Exception(..)
              ,throw
              ,reThrow
              ,unwrapExceptedValue
              )
       where


data Excepted a =
  Value a |
  Except Exception

data Exception =
  ExceptMessage String |
  ExceptLineMessage Int String |
  ExceptNoEndOfParens |
  ExceptBadBraces |
  ExceptBadFileName |
  ExceptBadUrl |
  ExceptBadTable

instance Functor (Excepted) where

  -- fmap :: (a -> b) -> (Excepted a -> Excepted b)
  fmap f (Value x)
    = Value (f x)
  fmap _ (Except e)
    = Except e


instance Applicative (Excepted) where
  
  -- (<*>) :: Excepted (a -> b) -> (Excepted a -> Excepted b)
  Value f <*> Value x = Value (f x)
  Except e <*> _ = Except e
  _ <*> Except e = Except e
  
  -- pure :: a -> Excepted a
  pure x
    = Value x
  

instance Monad (Excepted) where
  
  -- return :: a -> Excepted a
  return x
    = Value x

  -- >>= :: Excepted a -> (a -> Excepted b) -> Excepted b
  (Value x) >>= g
    = g x
  (Except e) >>= _
    = Except e



throw :: Exception -> Excepted a
throw e = Except e


reThrow :: Excepted a -> (Exception -> Exception) -> Excepted a
reThrow (Value x) _
  = Value x
reThrow (Except e) f
  = Except (f e)


exceptionMessage :: Exception -> String
exceptionMessage (ExceptMessage str)
  = str
exceptionMessage (ExceptLineMessage n str)
  = "Line " ++ show n ++ ". " ++ str
exceptionMessage _
  = "Unknown exception encountered."


unwrapExceptedValue :: Excepted a -> Either String a
unwrapExceptedValue (Value x)
  = Right x
unwrapExceptedValue (Except e)
  = Left (exceptionMessage e)
