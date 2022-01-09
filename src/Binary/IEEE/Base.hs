module Binary.IEEE.Base (
    Sign(..),
    IEEEFormat(..),
    IEEEFloat(..),
    IEEEFormatSpec(..)
) where

import Binary.Float.Base
-- | Formats:
--
-- * IEEESingle = float of 32 bits. [sign : 1bit][exponent : 8bits][mantissa : 23 bits]
--
-- * IEEEDouble = float of 64 bits. [sign :  bit][exponent :  bits][mantissa :    bits]
data IEEEFormat = IEEESingle | IEEEDouble deriving( Show , Eq , Ord , Enum )

-- | Normalized IEEE number
--
-- === Encoding
--
-- <------------ IEEEFormat -------------->
-- [getSign | getExponent | getSignificand]
data IEEEFloat a = IEEEFloat {
    getSign         :: Sign,        -- ^ Sign of the IEEE's number.
    getExponent     :: [a],         -- ^ Bits of the biased exponent.
    getSignificand  :: [a],         -- ^ Bits of the significand.
    format          :: IEEEFormat   -- ^ Encoded as...
} deriving( Show , Eq , Ord )

class IEEEFormatSpec format where
    -- | how many bits are used for the sign field.
    signSize        :: Integral size => format -> size
    signSize _ = 1

    -- | how many bits are used for the exponent field.
    exponentSize    :: Integral size => format -> size

    -- | how many bits are used for the significand field.
    significandSize :: Integral size => format -> size

instance IEEEFormatSpec IEEEFormat where
    exponentSize    IEEESingle = 8
    exponentSize    IEEEDouble = 11

    significandSize IEEESingle = 23
    significandSize IEEEDouble = 52
