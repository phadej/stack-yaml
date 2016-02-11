{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module StackYaml.Transformations.UpdateExtraDeps
    ( transformation
    ) where

import Control.Applicative           (many)
import Control.Exception             (IOException, catch)
import Control.Lens
import Data.Aeson.Lens
import Data.Monoid                   ((<>))
import Data.Text                     (Text, pack)
import Data.Version                  (showVersion)
import Data.Yaml                     (Value)
import System.Exit                   (exitFailure)
import System.IO                     (hPutStrLn, stderr)
import Text.Regex.Applicative.Common (decimal)
import Text.Regex.Applicative.Text   (anySym, few, match, sym)

import           Distribution.Package  (PackageIdentifier (..),
                                        PackageName (..))
import qualified Distribution.PackDeps as PackDeps
import           Distribution.Version  (Version (..))

transformation :: Value -> IO Value
transformation value = do
    newest <- catch PackDeps.loadNewest
                    (\e -> hPutStrLn stderr (failedToLoad e) *> exitFailure)
    traverseOf
      (key "extra-deps" . values . _String)
      (updateExtraDep newest)
      value
  where
    failedToLoad :: IOException -> String
    failedToLoad err =
      unlines
        [ show err
        , "Failed to read package database. Doing 'cabal update' might help."
        ]

    updateExtraDep :: PackDeps.Newest -> Text -> IO Text
    updateExtraDep newest dep =
        (pkgName <$> parsePackageIdentifier dep)
           & maybe
               (dep <$ hPutStrLn
                         stderr
                         ("Ignoring invalid extra-deps entry " <> show dep))
               (\name ->
                   newestPackageId newest name
                     & maybe
                         (dep <$ hPutStrLn
                                   stderr
                                   ("Ignoring extra-deps entry "
                                       <> show dep
                                       <> " as it refers to unknown package "
                                       <> show (unPackageName name)))
                         (pure . pPrintPackageIdentifier))

parsePackageIdentifier :: Text -> Maybe PackageIdentifier
parsePackageIdentifier = match pkgIdentifierRe
  where
    pkgIdentifierRe = PackageIdentifier
        <$> (PackageName <$> few anySym <* sym '-')
        <*> pkgVersionRe

    pkgVersionRe = flip Version [] <$> ((:) <$> decimal <*> many (sym '.' *> decimal))

pPrintPackageIdentifier :: PackageIdentifier -> Text
pPrintPackageIdentifier PackageIdentifier{..} =
    pack (unPackageName pkgName <> "-" <> showVersion pkgVersion)

newestPackageId :: PackDeps.Newest -> PackageName -> Maybe PackageIdentifier
newestPackageId newest name =
    PackDeps.diPackage <$> PackDeps.getPackage (unPackageName name) newest
