{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.SerializedData where

import           Control.Lens                      hiding ((*~))
import           Data.Typeable
import           Data.Word
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           Numeric.Units.Dimensional.Prelude



class IsVariable v where
  type VariableRep v :: *
  data Variable v :: *

  variableBits :: Variable v -> Int


-- | Temporary Speed Restriction
data V_TSR

instance IsVariable V_TSR where
  type VariableRep V_TSR = Velocity Double
  data Variable V_TSR = V_TSR Word8 deriving (Show, Eq, Typeable, Ord)
  variableBits _ = 7

_V_TSR :: (RealFrac a) => Prism' (Velocity a) (Variable V_TSR)
_V_TSR = prism' (\(V_TSR w) -> (Prelude.*) 5 (fromIntegral w) *~ kmh) b
  where b v =
          if (v < 0 *~ kmh) || (v > 600 *~ kmh) then Nothing
          else pure . V_TSR . flip div 5 . round $ v /~ kmh

