module Interval.Interval
( 
  Interval(..)
, End(..)
, EndType(..)
, Side(..)
, make_range
, inclusive_range
, exclusive_range
, infinite_range
, nil_range
, to_infinity
, from_infinity
, to_infinity_e
, from_infinity_e
, comp
, in_range
, overlap
) where

import Data.Function (on)

newtype Interval n = Interval { to_pair :: (End n, End n) }

data End n = End    { endtype :: EndType n
                    , side :: Side
                    }

instance (Show n) => Show (End n) where
    show (End Nil _)                = error "Nil outside of nil_range"
    show (End Infinity Low)         = "( Infinity"
    show (End Infinity High)        = "Infinity )"
    show (End (Inclusive x) Low)    = "[" ++ show x
    show (End (Exclusive x) Low)    = "(" ++ show x
    show (End (Inclusive x) High)   = show x ++ "]"
    show (End (Exclusive x) High)   = show x ++ ")"

instance (Show n) => Show (Interval n) where
    show (Interval ((End Nil _), (End Nil _)) ) = "( Nil )"
    show (Interval (e1, e2)) = show e1 ++ " - " ++ show e2

data EndType v = Inclusive v | Exclusive v | Infinity | Nil

data Side = Low | High

to_value :: End n -> n
to_value (End Infinity _)       = undefined
to_value (End Nil _)            = undefined
to_value (End (Inclusive x) _)  = x
to_value (End (Exclusive x) _)  = x

make_range :: End n -> End n -> Interval n
make_range e1 e2 = Interval (e1, e2)

inclusive_range, exclusive_range :: n -> n -> Interval n
inclusive_range l r = make_range (End (Inclusive l) Low) (End (Inclusive r) High)
exclusive_range l r = make_range (End (Exclusive l) Low) (End (Exclusive r) High)

infinite_range, nil_range :: Interval n
infinite_range = make_range (End Infinity Low) (End Infinity High)
nil_range = make_range (End Nil Low) (End Nil High)

to_infinity, from_infinity, to_infinity_e, from_infinity_e :: n -> Interval n
to_infinity n = make_range (End (Inclusive n) Low) (End Infinity High)
to_infinity_e n = make_range (End (Exclusive n) Low) (End Infinity High)

from_infinity n = make_range (End Infinity Low) (End (Inclusive n) High)
from_infinity_e n = make_range (End Infinity Low) (End (Exclusive n) High)

comp :: (Ord n) => n -> End n -> Bool
comp n (End Infinity _)             = True
comp n (End Nil _)                  = False
comp n (End (Inclusive x) Low)      = not $ n < x
comp n (End (Exclusive x) Low)      = x < n
comp n (End (Inclusive x) High)     = not $ x < n
comp n (End (Exclusive x) High)     = n < x

in_range :: (Ord n) => Interval n -> n -> Bool
in_range (Interval (l, h)) n = ((==) `on` comp n) l h

overlap :: (Ord n) => Interval n -> Interval n -> Bool
overlap r1@(Interval (ela, eha)) r2@(Interval (elb, ehb)) =
    check_a || check_b
        where   check_a = check elb || check ehb
                    where   check (End Infinity _)  = True
                            check e                 = in_range r1 $ to_value e
                check_b = check ela || check eha
                    where   check (End Infinity _)  = True
                            check e                 = in_range r2 $ to_value e