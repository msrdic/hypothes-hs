{-# LANGUAGE OverloadedStrings #-}
module HypothesisClient ( main
                        , search
                        , SearchFilter (..)
                        , OrderType (..)
                        , SortType (..) ) where

import Data.Configurator (Worth(Required), load)
import qualified Data.Configurator as DC

import Data.Text ( Text, concat, pack )
import Data.Text.Encoding ( encodeUtf8 )
import System.FilePath ((</>), (<.>))

import Network.Wreq (getWith, defaults, param, header)
import Control.Lens ((.~), (&))
import Network.Wreq.Lens (Options)

_getAuthToken :: FilePath -> IO (Maybe Text)
_getAuthToken path = do
  config <- load [Required path]
  DC.lookup config "token" :: IO (Maybe Text)

getAuthHeader :: Maybe Text -> IO Text
getAuthHeader t =
  return $ case t of
    Nothing -> error "Token not set in auth.config."
    Just token -> Data.Text.concat ["Bearer ", token]

data SearchFilter = User Text
                  | Group Text
                  | URI Text
                  | Order OrderType
                  | SearchAfter Text
                  | Sort SortType
                  | Limit Int

data OrderType = Asc | Desc
data SortType = SortByCreated
              | SortByUpdated
              | SortByGroup
              | SortByID
              | SortByUser

getParams :: [SearchFilter] -> Options
getParams = getParams' defaults
getParams' :: Options -> [SearchFilter] -> Options
getParams' d [] = d
getParams' d fs = foldl (\ d' f' -> d' & toParam f') d fs

toFullUsername :: Text -> Text
toFullUsername username = Data.Text.concat [username, "@hypothes.is"]

toParam :: SearchFilter -> Options -> Options
toParam (User u) = param "user" .~ [Data.Text.concat ["acct:", toFullUsername u]]
toParam (Group gid) = param "group" .~ [gid]
toParam (URI u) = param "uri" .~ [u]
toParam (Order o) = param "order" .~ [toOrder o]
toParam (SearchAfter a) = param "search_after" .~ [a]
toParam (Sort s) = param "sort" .~ [toSort s]
toParam (Limit i) = param "limit" .~ [pack $ show i]

toSort :: SortType -> Text
toSort SortByCreated = "created"
toSort SortByUpdated = "updated"
toSort SortByGroup = "group"
toSort SortByID = "id"
toSort SortByUser = "user"

toOrder :: OrderType -> Text
toOrder Asc = "asc"
toOrder Desc = "desc"

search filters = do
  token <- _getAuthToken ("conf" </> "auth" <.> "config")
  authHeader <- getAuthHeader token
  let params = getParams filters
  let opts = params & header "Authorization" .~ [encodeUtf8 authHeader]
  getWith opts "https://api.hypothes.is/api/search"


main :: IO ()
main = search [] >>= print
