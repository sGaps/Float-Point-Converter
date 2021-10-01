module Binary.Float.Show (
    -- Data type
    TextEffect(..),

    -- show utilities
    showBinFloatSimple,
    showBinFloatUnixUnderlined,
    showsBinFloat,

    -- Formatting
    unixUnderLine,
    silentLine,
    barOnLine,
    curlyOnLine,
    bracketOnLine
) where

import Binary.Float.Base

resetline = "\ESC[0m\STX"
underline = "\ESC[4m\STX"

data TextEffect = TextEffect String String deriving (Show,Eq)

unixUnderLine = TextEffect underline resetline
silentLine    = TextEffect ""  ""
barOnLine     = TextEffect "-" "-"
curlyOnLine   = TextEffect "{" "}"
bracketOnLine = TextEffect "[" "]"

showBinFloatSimple b         = showsBinFloat barOnLine     b ""
showBinFloatUnixUnderlined b = showsBinFloat unixUnderLine b ""

showsBinFloat :: (Show a) => TextEffect -> BinFloat a -> ShowS
showsBinFloat (TextEffect pre post) (BinFloat s is fs cs t) =
        showString "(Bin-Float " .
        integers                 .
        floatPoint               .
        showString " "           .
        trStatus                 .
        showString ")"
    where ignore     = showString ""
          sign       = if (s == Negative) then showChar '-' else ignore
          dot        = showChar '.'
          combine xs = foldr (.) ignore xs

          integers   = if null is
                        then showChar '0'
                        else (foldr (.) ignore . fmap shows . reverse) is
          floatings  = (combine . fmap shows) fs
          cyclics    = showString pre . combine (fmap shows cs) . showString post
          floatPoint = case (null fs, null cs) of
                            (True ,True) -> ignore
                            (False,True) -> dot . floatings
                            _            -> dot . floatings . cyclics
          trStatus   = case t of
                        Just ix -> showString "<truncated at " . shows ix . showChar '>'
                        _       -> showString ""

