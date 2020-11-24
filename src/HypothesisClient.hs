{-# LANGUAGE OverloadedStrings #-}
module HypothesisClient (main) where

import Data.Configurator (Worth(Required), load)
import qualified Data.Configurator as DC

import Data.Text ( Text )
import System.FilePath ((</>), (<.>))

_getAuthToken :: FilePath -> IO (Maybe Text)
_getAuthToken path = do
  config <- load [Required path]
  DC.lookup config "token" :: IO (Maybe Text)

main :: IO ()
main = _getAuthToken ("conf" </> "auth" <.> "config") >>= print
