{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hypothesis.Aeson () where

import Hypothesis.Base
import Data.Aeson ( ToJSON, FromJSON, parseJSON, (.:), withObject)

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
      RangeSelectorType        -> RangeSelector <$> o .: "type"
                                                <*> o .: "startOffset"
                                                <*> o .: "endOffset"
                                                <*> o .: "startContainer"
                                                <*> o .: "endContainer"
      TextPositionSelectorType -> TextPositionSelector <$> o .: "type"
                                                       <*> o .: "start"
                                                       <*> o .: "end"
      TextQuoteSelectorType    -> TextQuoteSelector <$> o .: "type"
                                                    <*> o .: "exact"
                                                    <*> o .: "prefix"
                                                    <*> o .: "suffix"
instance ToJSON Selector where