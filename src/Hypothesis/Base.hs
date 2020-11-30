{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypothesis.Base ( Annotation (..), Document (..), Links (..)
                      , Target (..), Selector (..), SelectorType (..)
                      , fromResult ) where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Aeson ( ToJSON, FromJSON, parseJSON, (.:), withObject
                  , Result ( Success, Error ) )

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

newtype Document = Document { title :: Maybe [Text] } deriving (Show, Generic)

data Links = Links { html :: Maybe Text
                   , incontext :: Maybe Text
                   , json :: Maybe Text
                   } deriving (Show, Generic)

data Target = Target { source :: Maybe Text
                     , selector :: Maybe [Selector]
                     } deriving (Show, Generic)

data SelectorType = RangeSelector | TextPositionSelector | TextQuoteSelector
                    deriving (Show, Generic)

data Selector = Selector { _type :: SelectorType
                         , exact :: Text
                         , prefix :: Text
                         , suffix :: Text }
               | NAS
                         deriving (Show, Generic)

-- this should go to an internal library
fromResult :: Result a -> a
fromResult (Success r) = r
fromResult (Error e) = error e

instance FromJSON Annotation where
instance ToJSON Annotation where

instance FromJSON Document where
instance ToJSON Document where

instance FromJSON Links where
instance ToJSON Links where

instance FromJSON Target where
instance ToJSON Target where

instance FromJSON SelectorType where
instance ToJSON SelectorType where

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