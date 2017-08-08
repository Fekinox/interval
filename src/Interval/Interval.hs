--  Interval Implementation
--      by William Owen Fowlkes

--  This is purely for educational purposes: far more useful and stable interval libraries exist.

module Interval.Interval
( 
--  Types
--  Interval: A pair of a lower and upper bound
--  Uses smart constructor interval to check for errors in creation.
  Interval(..)
--  Bound: a type of boundary for an interval
--      Bounded (v, True):  Inclusive (all values and itself)
--      Bounded (v, False): Exclusive (all values except itself)
--      PosInf:             all values less than itself
--      NegInf:             all values greater than itself
, Bound(..)
--  Constructors
--  Smart constructor
, interval
--  Unbounded interval
, infiniteInterval
--  Error interval with nonsensical bounds (posinf to neginf)
, nilInterval
-- Constructor operators
, (<=..<=), (<=..<), (<..<=), (<..<)
--  Interval going from/to some value to/from infinity
, toInfinityInc
, fromInfinityInc
, toInfinityExc
, fromInfinityExc
, isInc
, lowerBound, upperBound
, lowerBound', upperBound'
) where

import Data.Function (on)

-- Interval type

newtype Interval n = Interval { toPair :: (Bound n, Bound n) }
                        deriving (Eq)
-- Bound type

data Bound v    = NegInf
                | Bounded (v, Bool)
                | PosInf
                deriving (Ord, Eq)

-- Typeclass Instances

-- Show
instance (Show s) => Show (Bound s) where
    show PosInf             = "PosInf"
    show NegInf             = "NegInf"
    show (Bounded (v, i))   = show v ++ (if i then " inclusive" else " exclusive")

instance (Show s, Eq s, Ord s) => Show (Interval s) where
    show i@(Interval (lb, ub))
        | i == nilInterval = "( Nil )"
        | otherwise         =   let (                           lEdge, uEdge  )
                                     | isInc lb && isInc ub = ( "[ ",    " ]"   )
                                     | isInc lb             = ( "[ ",    " )"   )
                                     | isInc ub             = ( "( ",    " ]"   )
                                     | otherwise            = ( "( ",    " )"   )
                                in lEdge ++ show lb ++ " - " ++ show ub ++ uEdge

-- Functions

--  Creates an interval from two bounds
interval :: (Ord n) => Bound n -> Bound n -> Interval n
interval lb ub =
    case lb `compare` ub of
        GT -> Interval (ub, lb)     -- Swap the bounds if one is greater than another
        EQ -> nilInterval           -- Return a nilInterval if the bounds are equal
        LT -> Interval (lb, ub)     -- Return an interval with the given bounds

--  Converts a bound to its respective value
toValue :: Bound n -> n
toValue PosInf             = undefined
toValue NegInf             = undefined
toValue (Bounded (x, _))   = x

--  Shamelessly ripped off from Masahiro Sakai's Data.Interval

--  <= : inclusive
--  <  : exclusive
(<=..<=), (<=..<), (<..<=), (<..<) :: (Ord n) => n -> n -> Interval n
(<=..<=)    lb ub   = interval (inclusive lb)  (inclusive ub)
(<=..<)     lb ub   = interval (inclusive lb)  (exclusive ub)
(<..<=)     lb ub   = interval (exclusive lb)  (inclusive ub)
(<..<)      lb ub   = interval (exclusive lb)  (exclusive ub)

--  Some utility intervals
--  nilInterval is only returned from errors         
infiniteInterval, nilInterval :: (Ord n) => Interval n
infiniteInterval = interval NegInf PosInf
nilInterval = Interval (PosInf, NegInf)

--  Creates intervals going to and from infinity, exclusive or inclusive
toInfinityInc, fromInfinityInc, toInfinityExc, fromInfinityExc :: (Ord n) => n -> Interval n
toInfinityInc n = interval (inclusive n) PosInf
toInfinityExc n = interval (exclusive n) PosInf

fromInfinityInc n = interval NegInf (inclusive n) 
fromInfinityExc n = interval NegInf (exclusive n)

inclusive, exclusive :: n -> Bound n
inclusive n = Bounded (n, True)
exclusive n = Bounded (n, False)

--  Checking single values
--  These return an Ordering that describes the following:
--  GT: Above interval
--  EQ: In interval
--  LT: Below interval

-- Helper functions

isInc, isExc :: Bound n -> Bool
isInc (Bounded (_, b))  = b
isInc _                 = False
isExc = not . isInc

lowerBound, upperBound :: Interval n -> n
lowerBound (Interval (Bounded (v, _), _)) = v
upperBound (Interval (_, Bounded (v, _))) = v

lowerBound', upperBound' :: Interval n -> Bound n
lowerBound' (Interval (v, _)) = v
upperBound' (Interval (_, v)) = v