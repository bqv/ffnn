{-# LANGuagE FlexibleContexts #-}

module Util where

import Control.Monad.Writer

{- Logging -}

writeLogF :: (MonadWriter [String] m) => (a -> String) -> a -> m ()
writeLogF f x = tell [f x]

writeLog :: (MonadWriter [String] m) => String -> m ()
writeLog w = tell [w]

traceLogF :: (MonadWriter [String] m) => (a -> String) -> a -> m a
traceLogF f x = tell [f x] >>
                return x

traceLog :: (MonadWriter [String] m) => String -> a -> m a
traceLog w x = tell [w] >>
               return x

{- Util -}

head' :: [a] -> Maybe a
head' (x:xs) = Just x
head' [] = Nothing

tail' :: [a] -> Maybe [a]
tail' (x:xs) = Just xs
tail' [] = Nothing

init' :: [a] -> Maybe [a]
init' [] = Nothing
init' [x] = Just []
init' (x:xs) = fmap (x:) (init' xs)

last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

dot' :: (Num a) => [a] -> [a] -> Maybe a
dot' [] [] = Just 0
dot' (x:xs) (y:ys) = dot' xs ys >>= return . ((x*y) +)
dot' _ _ = Nothing

zipWith' :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWith' _ [] [] = Just []
zipWith' fn (x:xs) (y:ys) = zipWith' fn xs ys >>= return . (fn x y :)
zipWith' _ _ _ = Nothing

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> Maybe [d]
zipWith3' _ [] [] [] = Just []
zipWith3' fn (x:xs) (y:ys) (z:zs) = zipWith3' fn xs ys zs >>= return . (fn x y z :)
zipWith3' _ _ _ _ = Nothing
