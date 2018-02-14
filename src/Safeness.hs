{-# LANGUAGE InstanceSigs #-}

module Safeness
    ( Safeness(..)
    , isNotUnsafe
    , fmap
    , (<*>)
    , (>>=)
    )
    where


data Safeness a 
    = Unsafe a
    | Safe a
    | VerySafe a
    deriving (Eq, Show)


contents :: Safeness a -> a
contents s =
    case s of
        Unsafe x -> x
        Safe x -> x
        VerySafe x -> x


cap :: Safeness a -> (a -> Safeness a) -> Safeness a
cap cappee fn =
        let
            capper = fn $ contents cappee
        in
            case ( cappee,      capper ) of
                ( _,            Unsafe _ )      -> capper
                ( _,            VerySafe _ )    -> cappee
                ( VerySafe _,   Safe _ )        -> capper
                ( _,            _ )             -> cappee


isNotUnsafe :: Safeness a -> Bool
isNotUnsafe x =
    case x of
        Unsafe _ -> False
        _ -> True


instance Functor Safeness where
    fmap :: (a -> b) -> Safeness a -> Safeness b
    fmap fn (Unsafe x)      = Unsafe $ fn x
    fmap fn (Safe x)        = Safe $ fn x
    fmap fn (VerySafe x)    = VerySafe $ fn x


instance Applicative Safeness where
    pure :: a -> Safeness a
    pure = Unsafe -- play it "safe"

    (<*>) :: Safeness (a -> b) -> Safeness a -> Safeness b
    Safe fn     <*> Safe a      = Safe $ fn a
    VerySafe fn <*> VerySafe a  = VerySafe $ fn a
    VerySafe fn <*> Safe a      = Safe $ fn a
    Safe fn     <*> VerySafe a  = Safe $ fn a
    Unsafe fn   <*> Unsafe a    = Unsafe $ fn a
    Unsafe fn   <*> Safe a      = Unsafe $ fn a
    Unsafe fn   <*> VerySafe a  = Unsafe $ fn a
    Safe fn     <*> Unsafe a    = Unsafe $ fn a
    VerySafe fn <*> Unsafe a    = Unsafe $ fn a


instance Monad Safeness where    
    (>>=) :: Safeness a -> (a -> Safeness b) -> Safeness b
    Unsafe a    >>= fn = cap (fn a) Unsafe
    Safe a      >>= fn = cap (fn a) Safe
    VerySafe a  >>= fn = cap (fn a) VerySafe 