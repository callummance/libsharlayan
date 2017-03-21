-- |Module containing a lazy monadic takeWhile function
module Control.Monad.TakeWhileM
  where

import Control.Monad

-- |Given a perdicate and a list of monad elements, returns a list inside the monad of elements up until the first element which does not satisfy the predicate
takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM f [] = return []
takeWhileM f (x:xs)
  = do
    cur <- x
    case (f cur) of
      True      -> (liftM ((:) cur)) (takeWhileM f xs)
      False     -> return []
