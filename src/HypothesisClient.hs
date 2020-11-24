{-# LANGUAGE OverloadedStrings #-}
module HypothesisClient (main) where

import Data.Configurator (Worth(Required), load)
import qualified Data.Configurator as DC

import Data.Text ( Text, concat )
import Data.Text.Encoding ( encodeUtf8 )
import System.FilePath ((</>), (<.>))

import Network.Wreq ( getWith, defaults, param, header)
import Control.Lens ((.~), (&))

_getAuthToken :: FilePath -> IO (Maybe Text)
_getAuthToken path = do
  config <- load [Required path]
  DC.lookup config "token" :: IO (Maybe Text)

getAuthHeader :: Maybe Text -> IO Text
getAuthHeader t =
  return $ case t of
    Nothing -> error "Token not set in auth.config."
    Just token -> Data.Text.concat ["Bearer ", token]

main :: IO ()
main = do
  token <- _getAuthToken ("conf" </> "auth" <.> "config")
  authHeader <- getAuthHeader token
  let opts = defaults & param "user" .~ ["acct:msrdic@hypothes.is"]
                      & param "group" .~ ["__world__"]
                      & header "Authorization" .~ [encodeUtf8 authHeader]
  r <- getWith opts "https://api.hypothes.is/api/search"
  print r
