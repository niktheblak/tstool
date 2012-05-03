module Statistics(countElements,
                   calculateDistribution,
                   showTable) where

import Data.List(sort)
import qualified Data.Map as M

-- | Counts the instances of a specified element in a list. For example:
--
-- @
-- countElements 1 [0, 1, 1, 1, 2] = 3
-- @
countElements :: (Eq a) => a -> [a] -> Int
countElements x = length . filter (\n -> n == x)

-- | Generates a distribution map from the members of a list. The keys in the
-- resultant map contain each unique element of the list. The values contain
-- the number of times each element occurrs in the list. 
calculateDistribution :: (Ord a) => [a] -> M.Map a Int
calculateDistribution list = worker list M.empty where
    worker (n : ns) map =
        if M.member n map
            then
                let v = map M.! n in
                worker ns (M.insert n (v + 1) map)
            else worker ns (M.insert n 1 map)
    worker [] map = map

makeBar :: (Integral a) => a -> a -> String
makeBar n max =
    let n' = fromIntegral n
        max' = fromIntegral max
        r = n' / max'
        amount = round (r * 10)
    in
    replicate (fromIntegral amount) '*'

-- | Generates a string representation of a distribution table generated
-- by the 'calculateDistribution' function.
showTable :: (Ord k, Show k, Integral a) => M.Map k a -> String
showTable m =
    let keys = M.keys m
        s = sort keys
        max = maximum (M.elems m)
        printv k =
            let v = m M.! k in
            show k ++ " :\t" ++ show v ++ "\t" ++ makeBar v max
    in
    unlines (map printv s)
