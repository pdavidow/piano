module Safeness
    ( Safeness(..)
    , fmap
    )
    where


data Safeness a 
    = Unsafe a
    | Safe a
    | VerySafe a


instance Functor Safeness where
    fmap fn (Unsafe x) = Unsafe $ fn x
    fmap fn (Safe x) = Safe $ fn x
    fmap fn (VerySafe x) = VerySafe $ fn x