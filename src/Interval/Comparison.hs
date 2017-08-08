module Interval.Comparison
(
--  For all values x in X and y in Y, do the comparison operator
  (<=!), (<!), (>=!), (>!), (==!)
--  For all values x in X and y in Y, do the values in X lie within the values in Y?
) where

import Interval.Interval
import Interval.Check

(<=!), (<!), (>=!), (>!), (==!) :: (Ord n) => Interval n -> Interval n -> Bool

(<=!) iv1 iv2 =
    case ub1 `compare` lb2 of
        LT -> True
        GT -> False
        EQ ->
            case ub1 of
                NegInf -> True
                PosInf -> True
                Bounded _ -> not (in1 && in2)
        where   ub1 = upperBound' iv1
                lb2 = lowerBound' iv2
                in1 = isInc $ upperBound' iv1
                in2 = isInc $ lowerBound' iv2

(<!) iv1 iv2 = lowerBound' iv2 > upperBound' iv1 

(>=!) = flip (<=!)

(>!) = flip (<!)

(==!) iv1 iv2 = iv1 <=! iv2 && iv2 <=! iv1 