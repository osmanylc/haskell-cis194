{-# LANGUAGE 
  FlexibleInstances,
  TypeSynonymInstances
#-}

module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

import Data.List (foldl')

-- Exercise 1 --
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append ((tag x) <> (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2 --
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ 
  | i < 0        = Nothing
indexJ _ Empty   = Nothing
indexJ i jl@(Single m a)
  | i == 0       = Just a
  | otherwise    = Nothing
indexJ i (Append m l r)
  | i >= jlSize  = Nothing
  | i < leftSize = indexJ i l
  | otherwise    = indexJ (i - leftSize) r
  where leftSize = (getSize . size . tag) l
        jlSize   = (getSize . size) m

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
  | n <= 0           = jl
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append s l r) 
  | n >= jlSize      = Empty
  | n < leftSize     = dropJ n l
  | otherwise        = dropJ (n - leftSize) r
  where leftSize = (getSize . size . tag) l
        jlSize   = (getSize . size) s

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ 
  | n <= 0           = Empty
takeJ _ Empty        = Empty
takeJ _ (Single s x) = Single s x
takeJ n (Append s l r)
  | n >= jlSize      = Append s l r
  | n < leftSize     = takeJ n l
  | otherwise        = l +++ takeJ (n - leftSize) r
  where leftSize = (getSize . size . tag) l
        jlSize   = (getSize . size) s 

-- Exercise 3 --
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4 --
scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r

  fromString = (foldl' (+++) Empty) . (map scoreSizeLine) . lines

  line = indexJ

  replaceLine i _ jl 
    | i < 0 || i >= numLines jl = jl
  replaceLine _ ln (Single m s) = Single m ln
  replaceLine i ln jl@(Append m l r)
    | i < leftLines             = Append (tag newL <> tag r) newL r
    | otherwise                 = Append (tag l <> tag newR) l newR
    where newL = replaceLine i ln l
          newR = replaceLine (i - leftLines) ln r
          leftLines = numLines l


  numLines = getSize . snd . tag

  value = getScore . fst . tag

initBuff :: JoinList (Score, Size) String
initBuff = (fromString . unlines)
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L, followed"
         , "by the name of the file."
         ]
main = runEditor editor initBuff 
