{-# LANGUAGE OverloadedStrings #-}
module StackYaml.Normalize (normalize) where

import Control.Lens
import Data.Aeson.Lens (key, values, _String)
import Data.List       (nub, sortBy)
import Data.Ord        (comparing)
import Data.Yaml       (Value (..))

import qualified Data.Text as T

normalize :: Value -> Value
normalize v = v
    & partsOf (key "packages" . values) %~ nub . sortBy (comparing pkgName)
    & partsOf (key "extra-deps" . values . _String) %~ nub . sortBy (comparing T.toLower)
  where
    pkgName :: Value -> Either T.Text (Either T.Text ())
    pkgName (String s) = Left s
    pkgName (Object o) = case o ^? ix "location" . key "git" . _String of
        Just s  -> Right (Left s)
        Nothing -> Right (Right ())
    pkgName _          = Right (Right ())
