module Golf where

import Data.List

-- Exercise 1: HOPSCOTCH

-- Builds a list where the n-th element of the list is built by taking 
-- every nth element of xs. Passes in [(xs, 1), (xs, 2), ...] to 
-- filterSkips and returns [[xs_1, xs_2, ...], [xs_2, xs_4, ...], ...].
skips :: [a] -> [[a]]
skips xs = map filterSkips (zip (repeat xs) [1..length xs])


-- Takes every n elements of this list xs.
-- It does this by building a new list with the indices of every element 
-- of xs attached: [(1, xs_1), (2, xs_2), ...]. It then checks if the 
-- index is a multiple of n, and filters the list to only contain multiples
-- of n: [(n, xs_n), (2n, xs_2n), ...]. It then takes the second element 
-- of every tuple in the list, returning the desired filter.
filterSkips :: ([a], Int) -> [a]
filterSkips (xs, n) = map snd $ filter (fstMultipleN n) (zip [1..] xs)


-- Return true iff the first element of the 2-tuple is a multiple of n.
fstMultipleN :: Int -> (Int, a) -> Bool
fstMultipleN n (a, _) = a `mod` n == 0


-- Exercise 2: LOCAL MAXIMA

-- Transforms xs into a list of consecutive triples of elements of xs. Checks
-- if the middle element of each triple is the maximum element in the triple, 
-- and returns a list of all the middle elements that fit this condition.
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:xs) = map snd3 
    $ filter isMidMax $ zip3 (a:b:xs) (b:xs) xs
localMaxima _ = []


-- Check if the middle element of a 3-tuple of integers is greater than 
-- the other two.
isMidMax :: (Integer, Integer, Integer) -> Bool
isMidMax (a,b,c) = a < b && b > c


-- Return the second element of a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b


-- Exercise 3: HISTOGRAM

-- Make a histogram to print out in the command line.
-- First, a list of lists is made, grouping each digit into its own list. 
-- Then that list of lists is transposed, so that the nth inner list now 
-- contains the nth member of each of the previous inner lists. The nth 
-- inner list now represents the nth row of the histogram. Then, each inner 
-- list is converted into the string that will be the histogram row and the 
-- static base of the histogram is added. Finally, the order of the histogram 
-- rows is reverse so that the output is in the right order, and linebreaks 
-- are added at the end of each line.
histogram :: [Integer] -> String
histogram xs = (intercalate "\n" 
    $ reverse 
    $ histBase ++ map toHistLine ((transpose . group . sort) xs)
    ) ++ "\n"


-- The bottom two rows of the histogram.
histBase :: [String]
histBase = ["0123456789", replicate 10 '=']


-- Make a row of the histogram.
-- A list of integers marking where the asterisks will go is the input. 
-- The idea is to make individual strings of spaces that end with asterisks 
-- e.g. "   *", and then concatenating them together. The zipWith makes 
-- this easier by measuring the length of such asterisk strings, which 
-- toAsterisk takes care of converting into the actual string.
toHistLine :: [Integer] -> String
toHistLine xs = concat $ map toAsterisk $ zipWith (-) xs ((-1):xs)


-- Make a line of spaces that ends in asterisks of length n.
-- Makes a string of length n that has (n-1) spaces and an * at the end.
toAsterisk :: Integer -> String
toAsterisk n
    | n > 0 = replicate (fromIntegral (n-1)) ' ' ++ "*"
    | otherwise = []


