module Binary.IEEE.Show (
    showIEEE,
    showsIEEE
) where

import Binary.IEEE.Base

showIEEE ieee = showsIEEE ieee ""

showsIEEE :: Show a => IEEEFloat a -> ShowS
showsIEEE =
    showsGeneralIEEE sign exp sgnf
    where sign s = paren "[" ""  s
          exp  s = paren "|" "|" s
          sgnf s = paren ""  "] <without rounding>" s
          paren op cl sh = showString op .
                           sh            .
                           showString cl

-- | each (ShowS -> ShowS) must add parenthesis to its argument
showsGeneralIEEE sParen eParen mParen (IEEEFloat s es ms fmt) =
    prelude      .
    sign         .
    exponent     .
    significand  .
    end
    where ignore   = showString ""
          bitSign  = if (s == Negative) then [1] else [0]
          prelude  = showChar '(' . shows fmt . showChar ' '
          end      = showChar ')'

          sign        = bitField sParen . fmap shows $ bitSign
          exponent    = bitField eParen . fmap shows $ es
          significand = bitField mParen . fmap shows $ ms

          combine xs      = foldr (.) ignore xs
          bitField par xs = let body = combine xs
                             in par body
                                

