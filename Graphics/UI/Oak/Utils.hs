module Graphics.UI.Oak.Utils
       (
         nextAfter'
       , nextAfter
       , prevBefore'
       , prevBefore

       , mconcat
       ) where

import Control.Monad (MonadPlus, mplus, mzero)
import Data.Maybe (fromMaybe)
import Data.List (find, foldl')


nextAfter' :: (Eq a) => a -> (a -> Bool) -> [a] -> a
nextAfter' a f as =
  let (p, r) = break (== a) as
  in case r of
    []     -> a
    (_:xs) -> fromMaybe a $ find f (xs ++ p)

prevBefore' :: (Eq a) => a -> (a -> Bool) -> [a] -> a
prevBefore' a f as = nextAfter' a f $ reverse as

nextAfter :: (Eq a) => a -> [a] -> a
nextAfter a as = nextAfter' a (const True) as

prevBefore :: (Eq a) => a -> [a] -> a
prevBefore a as = prevBefore' a (const True) as


mconcat :: (MonadPlus m) => [m a] -> m a
mconcat = foldl' mplus mzero
