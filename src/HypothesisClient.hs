{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module HypothesisClient ( Annotation (..)
                        , SearchFilter (..)
                        , OrderType (..)
                        , SortType (..)
                        , search
                        , fetch ) where

import qualified Data.Configurator as DC
import GHC.Generics ( Generic )
import qualified Data.Vector as DV

import Data.Text ( Text, concat, pack, unpack )
import Data.Text.Encoding ( encodeUtf8 )
import System.FilePath ((</>), (<.>))
import Data.Aeson (ToJSON, (.:), withObject, Result(Success, Error), fromJSON, FromJSON, parseJSON)

import Network.Wreq (asValue, getWith, defaults, param, header, responseBody)
import Control.Lens ((^.), (.~), (&))
import Network.Wreq.Lens (Options)
import Data.Aeson.Lens (key, _Array)

_getAuthToken :: FilePath -> IO (Maybe Text)
_getAuthToken path = do
  config <- DC.load [DC.Required path]
  DC.lookup config "token" :: IO (Maybe Text)

getAuthHeader :: Maybe Text -> IO Text
getAuthHeader t =
  return $ case t of
    Nothing -> error "Token not set in auth.config."
    Just token -> Data.Text.concat ["Bearer ", token]

data SearchFilter = User Text
                  | Group Text
                  | URI Text
                  | URIParts Text
                  | Order OrderType
                  | SearchAfter Text
                  | Sort SortType
                  | Limit Int
                  | Any Text

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
toParam (URIParts t) = param "uri.parts" .~ [t]
toParam (Order o) = param "order" .~ [toOrder o]
toParam (SearchAfter a) = param "search_after" .~ [a]
toParam (Sort s) = param "sort" .~ [toSort s]
toParam (Limit i) = param "limit" .~ [pack $ show i]
toParam (Any t) = param "any" .~ [t]

toSort :: SortType -> Text
toSort SortByCreated = "created"
toSort SortByUpdated = "updated"
toSort SortByGroup = "group"
toSort SortByID = "id"
toSort SortByUser = "user"

toOrder :: OrderType -> Text
toOrder Asc = "asc"
toOrder Desc = "desc"

search :: [SearchFilter] -> IO [Annotation]
search filters = do
  token <- _getAuthToken ("conf" </> "auth" <.> "config")
  authHeader <- getAuthHeader token
  let params = getParams filters
  let opts = params & header "Authorization" .~ [encodeUtf8 authHeader]
  r <- getWith opts "https://api.hypothes.is/api/search"
  let rb = r ^. responseBody ^. key "rows" . _Array
  let items = map fromResult $ DV.toList $ DV.map fromJSON rb
  return items

fetch :: Text -> IO Annotation
fetch aid = do
  token <- _getAuthToken ("conf" </> "auth" <.> "config")
  authHeader <- getAuthHeader token
  let opts = defaults & header "Authorization" .~ [encodeUtf8 authHeader]
  r <- getWith opts ("https://api.hypothes.is/api/annotations/" ++ unpack aid) >>= asValue
  let rb = r ^. responseBody
  let annotation = fromResult $ fromJSON rb
  return (annotation :: Annotation)

data Annotation = Annotation { id :: Text
                             , created :: Text
                             , updated :: Text
                             , user :: Text
                             , uri :: Text
                             , text :: Text
                             , tags :: [Text]
                             , group :: Text
                             , target :: [Target]
                             , document :: Maybe Document
                             , links :: Links
                             } deriving (Show, Generic)

instance FromJSON Annotation where
instance ToJSON Annotation where

-- newtype because only one field?
newtype Document = Document { title :: Maybe [Text] } deriving (Show, Generic)

instance FromJSON Document where
instance ToJSON Document where

data Links = Links { html :: Maybe Text
                   , incontext :: Maybe Text
                   , json :: Maybe Text
                   } deriving (Show, Generic)

instance FromJSON Links where
instance ToJSON Links where

data Target = Target { source :: Maybe Text
                     , selector :: Maybe [Selector]
                     } deriving (Show, Generic)

instance FromJSON Target where
instance ToJSON Target where

data SelectorType = RangeSelector | TextPositionSelector | TextQuoteSelector
                    deriving (Show, Generic)

instance FromJSON SelectorType where
instance ToJSON SelectorType where

data Selector = Selector { _type :: SelectorType
                         , exact :: Text
                         , prefix :: Text
                         , suffix :: Text }
               | NAS
                         deriving (Show, Generic)

instance FromJSON Selector where
  parseJSON = withObject "selector" $ \o -> do
    t <- o .: "type"
    case t of
      RangeSelector        -> return NAS
      TextPositionSelector -> return NAS
      TextQuoteSelector    -> Selector <$> o .: "type"
                                       <*> o .: "exact"
                                       <*> o .: "prefix"
                                       <*> o .: "suffix"
instance ToJSON Selector where

fromResult :: Result a -> a
fromResult (Success r) = r
fromResult (Error e) = error e