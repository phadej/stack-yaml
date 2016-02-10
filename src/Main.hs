{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative         (many, (<|>))
import Control.Lens
import Control.Monad.Catch         (throwM)
import Data.Aeson.Lens
import Data.List                   (elemIndex)
import Data.Monoid                 ((<>))
import Data.Text                   (Text, pack)
import Data.Yaml                   (Object, Value, decodeFileEither)
import Data.Yaml.Pretty            (defConfig, encodePretty, setConfCompare)
import Text.Regex.Applicative.Text (match, psym)

import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import qualified GitHub              as GH
import qualified Options.Applicative as O

updateGithubDeps :: Value -> IO Value
updateGithubDeps =
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

data Opts = Opts
    { _optsCmd       :: Value -> IO Value
    , _optsStackFile :: FilePath
    }

execCmd :: Opts -> IO ()
execCmd (Opts f path) = do
    v' <- decodeFileEither path
    case v' of
        Left err -> throwM err
        Right v  -> f v >>= B.writeFile path . encodePretty cfg
  where
    cfg = setConfCompare cmp defConfig
    cmp = keyOrder ["resolver", "packages", "extra-deps", "flags", "git", "commit"] <> compare

keyOrder :: [Text] -> Text -> Text -> Ordering
keyOrder xs a b = case (elemIndex a xs, elemIndex b xs) of
    (Just i,  Just j)   -> compare i j
    (Just _,  Nothing)  -> LT
    (Nothing, Just _)   -> GT
    (Nothing, Nothing)  -> EQ

optsParser :: O.Parser Opts
optsParser = Opts <$> cmdParser
    <*> O.strOption (O.long "stack-yaml" <> O.metavar "STACK_YAML" <> O.help "Which stack.yaml file to use" <> O.value "stack.yaml" <> O.showDefault)

cmdParser :: O.Parser (Value -> IO Value)
cmdParser = O.subparser $ mconcat
    [ p "id" pure "Identity transformation"
    , p "update-github-deps" updateGithubDeps "Update github packages to the latest commit in master branch"
    ]
  where
    p :: String -> (Value -> IO Value)-> String -> O.Mod O.CommandFields (Value -> IO Value)
    p cmd commandActions desc =
         O.command cmd $ O.info (O.helper <*> pure commandActions) $ O.progDesc desc

main :: IO ()
main =
    O.execParser opts >>= execCmd
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "TBD"
        , O.header "stack.yaml modification tools"
        ]
