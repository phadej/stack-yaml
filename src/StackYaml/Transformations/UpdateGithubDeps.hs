{-# LANGUAGE OverloadedStrings #-}
module StackYaml.Transformations.UpdateGithubDeps
  ( transformation
  ) where

import Control.Applicative         (many, (<|>))
import Control.Lens
import Control.Monad.Catch         (throwM)
import Data.Aeson.Lens
import Data.Text                   (Text, pack)
import Data.Yaml                   (Object, Value)
import Text.Regex.Applicative.Text (match, psym)

import qualified Data.Vector as V
import qualified GitHub      as GH

transformation :: Value -> IO Value
transformation =
    traverseOf (key "packages" . values . key "location" . _Object) updateGithubDep
  where
    updateGithubDep :: Object -> IO Object
    updateGithubDep obj = case (obj ^? ix "git" . _String) >>= parseGithub of
      Nothing -> pure obj
      Just (owner, repo) -> do
          res <- GH.executeRequest' $ GH.branchesForR owner repo $ Just 1
          case res of
              Left err       -> throwM err
              Right branches -> case V.find ((== "master") . GH.branchName) branches of
                  Nothing     -> pure obj
                  Just branch -> let obj' = obj & ix "commit" . _String .~ GH.branchCommitSha (GH.branchCommit branch)
                                 in pure obj'

parseGithub :: Text -> Maybe (GH.Name GH.Owner, GH.Name GH.Repo)
parseGithub = match githubRe
  where
    githubRe = f <$ ("git@" <|> "https://") <* "github.com/" <*> ident <* "/" <*> ident <* ".git"
    ident    = pack <$> many (psym (/= '/'))
    f a b    = (GH.mkOwnerName a, GH.mkRepoName b)
