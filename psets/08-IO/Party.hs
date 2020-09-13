{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.List (sort)

-- Exercise 1 --
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (empFun emp + fun)

instance Semigroup GuestList where
  (GL e1 f1) <> (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

-- Exercise 2 --
treeFold :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts) 

-- Exercise 3 --
nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel boss gls = ((glCons boss) . mconcat . (map snd) $ gls
                     , mconcat . (map (uncurry max)) $ gls)

-- Exercise 4 --
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry max (treeFold nextLevel t)

-- Exercise 5 --
instance Show GuestList where
  show (GL emps fun)
    = "Total fun: " ++ show fun ++ "\n"
      ++ (unlines . sort . (map empName) $ emps)

main = readFile "company.txt"
        >>= putStrLn . show . maxFun . read
