{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module StackYaml.Transformations.CopyPackages
    ( transformation
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Data.Aeson.Lens
import Data.Yaml                     (Value)

transformation :: Value -> Value -> IO Value
transformation fromValue value = return newValue
  where
    fromPkgs = fromValue ^? key "packages"
    newValue  = value & key "packages" %~ maybe id const fromPkgs
