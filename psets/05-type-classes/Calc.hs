{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = 
  case parseExp Lit ExprT.Add ExprT.Mul s of
    Nothing  -> Nothing
    Just exp -> Just (eval exp)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
 
instance Expr ExprT where
  lit x = Lit x
  mul x y = ExprT.Mul x y
  add x y = ExprT.Add x y

-- Exercise 4
instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>= 0)
  mul = (&&)
  add = (||)

instance Expr MinMax where
  lit = MinMax . id
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5
instance Expr Program where
  lit x = [PushI x]
  mul x y = x ++ y ++ [StackVM.Mul]
  add x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul


-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VVar String
              | VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add x y m = case (x m, y m) of 
                (Just a, Just b) -> Just (a + b)
                _ -> Nothing
  mul x y m = case (x m, y m) of 
                (Just a, Just b) -> Just (a * b)
                _ -> Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

