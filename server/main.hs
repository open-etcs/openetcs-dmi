{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Main ( main ) where

#if __GHCJS__

main :: IO ()
main = return ()

#else

import           Control.Lens hiding ((*~))
import           Control.Monad
import           Control.Monad.IO.Class
import           ETCS.DMI
import           ETCS.Binding.Raildriver
import           ETCS.Bindings
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
import Reflex

mkBinding :: IO (TrainBehavior Spider)
mkBinding = initBinding $ RailDriverBinding "localhost" 22222

  
main :: IO ()
main = do
  tb <- mkBinding
  return ()

#endif

