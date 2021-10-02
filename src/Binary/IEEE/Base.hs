module Binary.IEEE.Base (
    Sign(..),
    IEEEFormat(..),
    IEEEFloat(..)
) where

import Binary.Float.Base
-- | Formats:
--
-- * IEEESingle = float of 32 bits. [sign : 1bit][exponent : 8bits][mantissa : 23 bits]
--
-- * IEEEDouble = float of 64 bits. [sign :  bit][exponent :  bits][mantissa :    bits]
data IEEEFormat = IEEESingle | IEEEDouble deriving( Show , Eq , Ord , Enum )

-- Normalized IEEE number
data IEEEFloat a = IEEEFloat {
    getSign         :: Sign,        -- ^ Sign of the IEEE's number.
    getExponent     :: [a],         -- ^ Bits of the biased exponent.
    getSignificand  :: [a],         -- ^ Bits of the significand.
    format          :: IEEEFormat   -- ^ Encoded as...
} deriving( Show , Eq , Ord )

