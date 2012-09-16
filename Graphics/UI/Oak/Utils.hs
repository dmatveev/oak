module Graphics.UI.Oak.Utils
       (
         nextAfter'
       , nextAfter
       , prevBefore'
       , prevBefore

       , mconcat

       , Stack
       , stack
       , push
       , pop

       , currentSeconds

       , mapmap
       , mapmapM
       , for
       ) where

import Control.Monad (MonadPlus, mplus, mzero, mapM)
import Data.Maybe (fromMaybe)
import Data.List (find, foldl')
import System.Time (ClockTime(..), getClockTime)


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


newtype Stack a = Stack [a]
                  deriving (Eq, Show)

stack :: Stack a
stack = Stack []

push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a : as)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (a:as)) = (Just a, Stack as)


currentSeconds :: IO Integer
currentSeconds = do
  (TOD sec _) <- getClockTime
  return sec


mapmap :: (a -> b) -> [[a]] -> [[b]]
mapmap f = map (map f)

mapmapM :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapmapM f = mapM (mapM f)

for :: [a] -> (a -> b) -> [b]
for = flip map
