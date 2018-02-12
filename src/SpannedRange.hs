module SpannedRange
    ( SpannedRange (..)
    , SpannedRange.inRange 
    )
    where

-- Motivation: Want to be able to safely do "(Data.Range.Range.SpanRange start end) = range" (see https://elmlang.slack.com/archives/C1UGSUNCX/p1518399834000081)

import qualified Data.Range.Range as Range ( Range(..), inRange ) 

data SpannedRange a = SpannedRange a a    


inRange :: Ord a => SpannedRange a -> a -> Bool
inRange (SpannedRange start end) x =
    Range.inRange range x where
        range = Range.SpanRange start end