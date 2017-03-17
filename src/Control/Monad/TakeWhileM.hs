module Control.Monad.TakeWhileM
  where

import Control.Monad

takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM f [] = return []
takeWhileM f (x:xs)
  = do
    cur <- x
    case (f cur) of
      True      -> (liftM ((:) cur)) (takeWhileM f xs)
      False     -> return []
