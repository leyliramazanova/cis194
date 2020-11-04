{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.List

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

-- Given the definitions
-- type Army = Int
-- data Battlefield = Battlefield { attackers :: Army, defenders :: Army } 
-- (which are also included in Risk.hs), write a function with the type
-- battle :: Battlefield -> Rand StdGen Battlefield
-- which simulates a single battle (as explained above) between two opposing armies. 
-- That is, it should simulate randomly rolling the appropriate number of dice, interpreting the results, and updating the two armies to reflect casualties. 
-- You may assume that each player will attack or defend with the maximum number of units they are allowed.

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

sorting :: Ord a => [a] -> [a]
sorting ls1 = sortBy (flip compare) ls1

sortAndPair :: Ord a => [a] -> [a] -> [(a,a)]
sortAndPair ls1 ls2 = zip (sorting ls1) (sorting ls2)

numberAttacking :: Army -> Army
numberAttacking a = min (a-1)  3

numberDefending :: Army -> Army
numberDefending a = min a 2

outcome :: (DieValue, DieValue) -> (Army, Army) -> (Army, Army)
outcome (attack_dieval, defend_dieval) (attack_army, defend_army) 
    | attack_dieval > defend_dieval = (attack_army, defend_army - 1)
    | otherwise = (attack_army - 1, defend_army)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = do
    aRolls <- dice (numberAttacking attackers)  
    dRolls <- dice (numberDefending defenders)
    let res = foldr outcome (attackers, defenders) $ sortAndPair aRolls dRolls
    return $ uncurry Battlefield res

-- Now implement a function
-- invade :: Battlefield -> Rand StdGen Battlefield
-- which simulates an entire invasion attempt, that is, 
-- repeated calls to battle until there are no defenders remaining, or fewer than two attackers.

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield attackers defenders) 
    | attackers < 2 || defenders <= 0 = return b
    | otherwise = battle b >>= invade


-- Finally, implement a function
-- successProb :: Battlefield -> Rand StdGen Double
-- which runs invade 1000 times, and uses the results to compute a Double between 0 and 1 
-- representing the estimated probability that the attacking army will completely destroy the defending army.

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    res <- sequence $ replicate 1000 (invade b)
    let finalCount = length $ filter (==True) (won <$> res)
    return $ fromIntegral finalCount / 1000
        where
            won (Battlefield attackers defenders) = attackers > defenders

