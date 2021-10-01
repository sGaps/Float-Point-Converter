module Binary.IEEE.Show (
    showIEEE,
    showsIEEE
) where

import Binary.IEEE.Base

showIEEE ieee = showsIEEE ieee ""

showsIEEE (IEEEFloat s es ms fmt) =
    showChar '(' .
    shows    fmt .
    showChar ' ' .
    sign         .
    exponent     .
    significand  .
    showChar ')'

    where ignore   = showString ""
          bitSign  = if (s == Negative) then [1] else [0]

          sign        = bitField "sign"        . fmap shows $ bitSign
          exponent    = bitField "exponent"    . fmap shows $ es
          significand = bitField "significand" . fmap shows $ ms

          combine xs       = foldr (.) ignore xs
          bitField name xs = let body   = combine xs
                                in showString "["   .
                                   showString name  .
                                   showString ": "  .
                                   body             .
                                   showString " :]"
        
