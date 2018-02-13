module TextPhrase
    ( TextPhrase(..)
    , Style(..)
    , make
    , styles
    , elagantize
    , emphasize
    )
    where

import Data.Set as Set (Set, empty, insert, delete, toList )
import Lib ( Emphasized, Elagantized, emphasize, elagantize )


data TextPhrase = TextPhrase (Set Style) String deriving (Eq)


data Style 
    = Bold
    | Italic
    | Underline
    | StrikeThrough
    deriving (Eq, Ord, Show)


instance Show TextPhrase where
    show (TextPhrase xs string) = 
        show string ++ " " ++ (show $ toList xs)


make :: String -> TextPhrase
make s =
    TextPhrase Set.empty s


styles :: TextPhrase -> [Style]
styles (TextPhrase xs _) = toList xs


adjustStyle :: Style -> Bool -> TextPhrase -> TextPhrase
adjustStyle x flag (TextPhrase xs string) =
    let
        fn = if flag then insert else delete
        xs' = fn x xs
    in
        TextPhrase xs' string


instance Emphasized TextPhrase where
    emphasize flag x = 
        adjustStyle Bold flag x


instance Elagantized TextPhrase where
    elagantize flag x = 
        adjustStyle Italic flag x
