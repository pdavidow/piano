module CS194
    ( ordPairs )
    where

--http://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html


ordPairs :: Ord a => [a] -> [(a, a)]
ordPairs xs =
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    
    -- xs >>= \x1 ->
    -- xs >>= \x2 ->
    -- if x1 < x2 then [(x1,x2)] else []

    let
        f :: Ord a => a -> a -> [(a,a)]
        f x1 x2 = if x1 < x2 then [(x1,x2)] else []
    in
        xs >>= (\x1 -> (xs >>= \x2 -> f x1 x2))