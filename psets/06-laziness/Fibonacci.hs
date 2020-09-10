{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1 --
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 --
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a+b)) (0, 1)

-- Exercise 3 --
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . (take 20) . streamToList

-- Exercise 4 --
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Stream z (streamFromSeed f (f z))

-- Exercise 5 --
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr interleaveStreams undefined $ map streamRepeat [0..]

-- Exercise 6 --
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate = streamMap negate
  (+) (Stream a0 a') (Stream b0 b') = Stream (a0 + b0) (a' + b')
  (*) a@(Stream a0 a') b@(Stream b0 b') = Stream (a0*b0) (a0b' + a'*b)
    where a0b' = streamMap (*a0) b'

instance Fractional (Stream Integer) where
  (/) a@(Stream a0 a') b@(Stream b0 b') = Stream (a0 `div` b0) rest
    where rest = streamMap (`div` b0) (a' - q*b')
          q = a / b

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7 --
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = 
    Matrix (a00*b00 + a01*b10) 
           (a00*b01 + a01*b11) 
           (a10*b00 + a11*b10) 
           (a10*b01 + a11*b11)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (\(Matrix _ x _ _) -> x) $ (Matrix 1 1 1 0)^n
