module SpannedRange
    ( SpannedRange (..)
    , SpannedRange.inRange 
    )
    where

-- Motivation: Want to be able to safely do "(Data.Range.Range.SpanRange start end) = range" (see https://elmlang.slack.com/archives/C1UGSUNCX/p1518399834000081)

import Data.Range.Range ( Range(..), inRange )


data SpannedRange a = SpannedRange a a    


inRange :: Ord a => SpannedRange a -> a -> Bool
inRange (SpannedRange start end) x =
    Data.Range.Range.inRange range x where
        range = Data.Range.Range.SpanRange start end