{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module StackYaml.Transformations.UpdateGithubDeps
  ( transformation
  ) where

import Prelude        ()
import Prelude.Compat

import Control.Applicative         (many, (<|>))
import Control.Lens
import Control.Monad.Catch         (throwM)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.Trans.Maybe   (MaybeT (..))
import Data.Aeson.Lens
import Data.Maybe                  (fromMaybe)
import Data.Text                   (Text, pack)
import Data.Yaml                   (Object, Value)
import Network.HTTP.Client         (Manager, newManager)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Text.Regex.Applicative.Text (match, psym)

import qualified Data.Vector as V
import qualified GitHub      as GH

transformation :: Value -> IO Value
transformation value = do
    manager <- newManager tlsManagerSettings
    traverseOf (key "packages" . values . key "location" . _Object) (updateGithubDep manager) value
  where
    updateGithubDep :: Manager -> Object -> IO Object
    updateGithubDep mgr obj = fmap (fromMaybe obj) . runMaybeT $ do
        (owner, repo) <- liftMaybe $ (obj ^? ix "git" . _String) >>= parseGithub
        branches      <- liftIO    $ executeRequest mgr $ GH.branchesForR owner repo $ Just 1
        branch        <- liftMaybe $ V.find ((== "master") . GH.branchName) branches
        let obj'      =  obj & ix "commit" . _String .~ GH.branchCommitSha (GH.branchCommit branch)
        pure obj'

liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

executeRequest :: Manager -> GH.Request 'False a -> IO a
executeRequest mgr req = either throwM pure =<< GH.executeRequestWithMgr' mgr req

parseGithub :: Text -> Maybe (GH.Name GH.Owner, GH.Name GH.Repo)
parseGithub = match githubRe
  where
    githubRe = f <$ ("git@" <|> "https://") <* "github.com/" <*> ident <* "/" <*> ident <* ".git"
    ident    = pack <$> many (psym (/= '/'))
    f a b    = (GH.mkOwnerName a, GH.mkRepoName b)
