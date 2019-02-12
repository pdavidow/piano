module MusicPhrase
    ( MusicPhrase(..)
    , Style(..)
    , style
    , elagantize
    , emphasize
    )
    where

import Instrument ( Instrument, isPolyphonic )
import Triad ( Triad )

data MusicPhrase = MusicPhrase Instrument Style Triad deriving (Eq, Show)


data Style 
    = None
    | Octave
    | Chord
    | ChordedOctave
    | Arpeggio
    | ArpeggiatedRun
    deriving (Eq, Ord, Show)


style :: MusicPhrase -> Style
style (MusicPhrase _ s _) = s


adjustStyle :: Style -> Bool -> MusicPhrase -> MusicPhrase
adjustStyle newStyle flag (MusicPhrase instrument style triad) =
    MusicPhrase instrument style' triad
        where style' = if flag then newStyle else None


elagantize :: Bool -> MusicPhrase -> MusicPhrase
elagantize flag x@(MusicPhrase instrument _ _) = 
    adjustStyle (elagantizedStyle instrument) flag x

            
emphasize :: Bool -> MusicPhrase -> MusicPhrase
emphasize flag x@(MusicPhrase instrument _ _) = 
    adjustStyle (emphasizedStyle instrument) flag x


elagantizedStyle :: Instrument -> Style
elagantizedStyle i =
    ArpeggiatedRun


emphasizedStyle :: Instrument -> Style
emphasizedStyle i =
    if isPolyphonic i then
        ChordedOctave
    else
        Arpeggio