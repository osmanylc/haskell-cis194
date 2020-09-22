{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Foreign.Marshal.Utils (fromBool)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2 --
battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield att def) = 
  let battleAtt = min (att - 1) 3
      battleDef = min def 2
  in rollN battleAtt                        >>= \attRolls               ->
     rollN battleDef                        >>= \defRolls               ->
     return (countDeaths attRolls defRolls) >>= \(attDeaths, defDeaths) ->
     return $ Battlefield (att - attDeaths) (def - defDeaths)

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die

countDeaths :: [DieValue] -> [DieValue] -> (Army, Army)
countDeaths attRolls defRolls = 
  let battleResults = zipWith (<=)
                              (reverse . sort $ attRolls)
                              (reverse . sort $ defRolls)
      deadDef       = length $ filter id battleResults
      deadAtt       = length battleResults - deadDef
  in (deadAtt, deadDef)

-- Exercise 3 --
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def) 
  | att < 2 || def == 0 = return bf
  | otherwise           = battle bf >>= invade

-- Exercise 4 --
attWinRate :: Foldable t => t Battlefield -> Double
attWinRate bfs =
  let wins  = foldl' (\z -> (+z) . fromBool . (== 0) . defenders) 0 bfs
      total = length bfs
  in fromIntegral wins / fromIntegral total

successProb :: Battlefield -> Rand StdGen Double 
successProb bf = 
  replicateM 1000 (invade bf) >>= \bfs ->
  return $ attWinRate bfs

main :: IO ()
main = evalRandIO (successProb (Battlefield 3 3)) >>= \w ->
       putStrLn $ show w
