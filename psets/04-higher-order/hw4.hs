import Data.List ((\\))

-- Exercise 1 --
fun1' :: [Integer] -> Integer
fun1' = product . map (-2 +) . filter even


fun2' :: Integer -> Integer
fun2' = sum 
      . filter even 
      . takeWhile (/= 1) 
      . iterate (\x -> if even x then x `div` 2 else 3*x + 1) 

-- Exercise 2 --
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = 
  Node 0 Leaf x Leaf
insert x (Node h left y right)
  | height left > height right
    = Node (1 + height left) left y (insert x right)
  | height left < height right
    = Node (1 + height right) (insert x left) y right
  | otherwise
    = Node (1 + height (insert x left)) (insert x left) y right
  where
    height Leaf = -1
    height (Node h _ _ _) = h

-- Exercise 3 __
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4 --
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys] 

sieveOfSundaram :: Integer -> [Integer]
sieveOfSundaram n = map (\x -> 2*x + 1)
                  . ([1..n] \\)
                  . filter (<= n)
                  . map (\(i,j) -> i + j + 2*i*j)
                  . filter (uncurry (<=))
                  $ [(x,y) | x <- [1..n], y <- [1..n]] 

