module Binary.Integer.Show (
    showBinInteger,
    showsBinInteger
) where

import Binary.Integer.Base

-- | Converts a BinInteger into a String
--
-- See also: 'showsBinInteger'
showBinInteger :: Show a => BinInteger a -> String
showBinInteger b = showsBinInteger b ""

-- | Converts a BinInteger into a composable ShowS with the following format:
-- 
-- >>> showsBinInteger (BinInteger Negative [0,1,0,0,1,1] 6) ""
-- -110010
showsBinInteger :: Show a => BinInteger a -> ShowS
showsBinInteger (BinInteger s ds) =
    sign   .
    digits
    where ignore = showString ""
          sign   = if (s == Negative) then showChar '-' else ignore
          digits = if null ds
                    then showChar '0'
                    else (foldr (.) ignore . fmap shows . reverse) ds   -- concatenates all digits in the right order.
                                                                        -- (bigger magnitude <element>) . (lower magniutde <accumulator>)
