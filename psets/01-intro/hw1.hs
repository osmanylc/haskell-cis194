toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = (reverse . toDigits) n 


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = (reverse . doubleRev . reverse)
    where doubleRev (a:b:ns) = a:2*b:doubleRev ns
          doubleRev ns = ns


sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits 


validate :: Integer -> Bool
validate = (==) 0 . (flip mod) 10 . sumDigits 
    . doubleEveryOther . toDigits


type Peg = String
type Move = (Peg, Peg)
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 _ _ _ = []
hanoi3 n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b c d = [(a,b)]
hanoi4 n a b c d = hanoi4 (n-2) a d b c ++ [(a,c), (a,b), (c,b)] 
    ++ hanoi4 (n-2) d b a c

