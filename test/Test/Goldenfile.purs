module Test.Goldenfile where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (difference)
import Data.Foldable (null)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Options.Applicative.Internal.Utils (unLines)
import Test.Spec.Assertions (fail)
import Text.Pretty (indent)

shouldEqualFile :: String -> FilePath -> Aff Unit
shouldEqualFile actual expectedFilePath = do
  envRegolden <- getEnvRegolden
  Console.log $ "REGOLDEN = " <> show envRegolden
  exists expectedFilePath # liftEffect >>= case _ of
    false -> do
      -- if goldenfile DOESN'T exist, then write it with actual text
      writeTextFile UTF8 expectedFilePath actual
    true -> do
      let actualLines = actual # split (Pattern "\n")
      expected <- readTextFile UTF8 expectedFilePath
      let expectedLines = expected # split (Pattern "\n")
      let deltas = difference actualLines expectedLines

      unless (null deltas) do
        if envRegolden then do
          Console.log $ "Updating goldenfile: " <> show expectedFilePath
          writeTextFile UTF8 expectedFilePath actual
        else
          fail $ unLines
            [ "Actual text differed from contents of " <> show expectedFilePath <> ":"
            , indent (unLines deltas)
            ]

getEnvRegolden :: forall m. MonadError Error m => MonadEffect m => m Boolean
getEnvRegolden = do
  v <- liftEffect $ lookupEnv keyEnvRegolden # map (fromMaybe "false")
  if v == "true" then pure true
  else if v == "false" then pure false
  else throwError $ error $ "Unexpected value for " <> keyEnvRegolden <> ": " <> show v

keyEnvRegolden :: String
keyEnvRegolden = "REGOLDEN"

