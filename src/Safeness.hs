module Safeness
    ( Safeness(..)
    , isNotUnsafe
    , fmap
    , (<*>)
    )
    where


data Safeness a 
    = Unsafe a
    | Safe a
    | VerySafe a


isNotUnsafe :: Safeness a -> Bool
isNotUnsafe x =
    case x of
        Unsafe _ -> False
        _ -> True


instance Functor Safeness where
    fmap fn (Unsafe x) = Unsafe $ fn x
    fmap fn (Safe x) = Safe $ fn x
    fmap fn (VerySafe x) = VerySafe $ fn x


instance Applicative Safeness where
    pure = Safe

    Safe fn     <*> Safe a      = Safe $ fn a
    VerySafe fn <*> VerySafe a  = VerySafe $ fn a
    VerySafe fn <*> Safe a      = Safe $ fn a
    Safe fn     <*> VerySafe a  = Safe $ fn a
    Unsafe fn   <*> Unsafe a    = Unsafe $ fn a
    Unsafe fn   <*> Safe a      = Unsafe $ fn a
    Unsafe fn   <*> VerySafe a  = Unsafe $ fn a
    Safe fn     <*> Unsafe a    = Unsafe $ fn a
    VerySafe fn <*> Unsafe a    = Unsafe $ fn a