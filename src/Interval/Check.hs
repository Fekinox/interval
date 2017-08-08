module Interval.Check
(
-- Locate a value in an interval
  (<?<)
, (<=<)
-- Locate a list of values in an interval
, locateValues
, withinValues
-- Locate a value in a set of intervals
, locateIntervals
, withinIntervals
) where

import Interval.Interval

(<?<) :: (Ord n) => n -> Interval n -> Ordering
(<?<) v (Interval (lb, ub)) =
    case ((v `above` lb), (v `below` ub)) of
        (True, True)    -> EQ
        (True, False)   -> GT
        (False, True)   -> LT
        (False, False)  -> error "Corrupt interval"
        where   above v PosInf = False
                above v NegInf = True
                above v (Bounded (b, i))
                    | i         = comp == GT || comp == EQ
                    | otherwise = comp == GT
                    where comp = v `compare` b
                below v PosInf = True
                below v NegInf = False
                below v (Bounded (b, i))
                    | i         = comp == GT || comp == EQ
                    | otherwise = comp == GT
                    where comp = b `compare` v

(<=<) :: (Ord n) => n -> Interval n -> Bool
(<=<) v iv = v <?< iv == EQ

locateValues :: (Ord n) => [n] -> Interval n -> [Ordering]
locateValues xs iv = foldr (\x acc -> x <?< iv : acc) [] xs

withinValues :: (Ord n) => [n] -> Interval n -> [Bool]
withinValues xs iv = foldr (\x acc -> x <=< iv : acc) [] xs

locateIntervals :: (Ord n) => n -> [Interval n] -> [Ordering]
locateIntervals x ivs = foldr (\iv acc -> x <?< iv : acc) [] ivs

withinIntervals :: (Ord n) => n -> [Interval n] -> [Bool]
withinIntervals x ivs = foldr (\iv acc -> x <=< iv : acc) [] ivs