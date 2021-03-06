module SpannedRange
    ( SpannedRange (..)
    , SpannedRange.inRange 
    )
    where

-- Motivation: Safely perform "(Data.Range.Range.SpanRange start end) = range"

import qualified Data.Range.Range as Range ( Range(..), inRange ) 

data SpannedRange a = SpannedRange a a    


inRange :: Ord a => SpannedRange a -> a -> Bool
inRange (SpannedRange start end) x =
    Range.inRange range x where
        range = Range.SpanRange start end