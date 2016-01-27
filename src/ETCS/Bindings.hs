{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module ETCS.Bindings where

import           ETCS.DMI.Types
import           Reflex

class (Reflex t) => TrainBehaviorBinding t a where
  data TrainBehaviorBindingConfig a :: *
  initBinding :: TrainBehaviorBindingConfig a -> IO (TrainBehavior t)


