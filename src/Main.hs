{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Catch (throwM)
import Data.List           (elemIndex)
import Data.Monoid         ((<>))
import Data.Text           (Text)
import Data.Yaml           (Value, decodeFileEither)
import Data.Yaml.Pretty    (defConfig, encodePretty, setConfCompare)

import qualified Data.ByteString     as B
import qualified Options.Applicative as O

import StackYaml.Normalize (normalize)

import qualified StackYaml.Transformations.UpdateExtraDeps  as UpdateExtraDeps
import qualified StackYaml.Transformations.UpdateGithubDeps as UpdateGithubDeps

data Opts = Opts
    { _optsCmd       :: Value -> IO Value
    , _optsStackFile :: FilePath
    }

execCmd :: Opts -> IO ()
execCmd (Opts f path) = do
    v' <- decodeFileEither path
    case v' of
        Left err -> throwM err
        Right v  -> f v >>= B.writeFile path . encodePretty cfg . normalize
  where
    cfg = setConfCompare cmp defConfig
    cmp = keyOrder ["resolver", "packages", "extra-deps", "flags", "git", "commit", "location", "extra-dep"] <> compare

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
    , p "update-github-deps"
        UpdateGithubDeps.transformation
        "Update github packages to the latest commit in master branch"
    , p "update-extra-deps"
        UpdateExtraDeps.transformation
        "Update extra-deps to the newest version in cabal database"
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
