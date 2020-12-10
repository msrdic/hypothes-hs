{-# LANGUAGE DeriveGeneric #-}
module Hypothesis.Base ( Annotation (..), Document (..), Links (..)
                      , Target (..), Selector (..), SelectorType (..)) where

import GHC.Generics ( Generic )
import Data.Text ( Text )

data Annotation = Annotation { id :: !Text
                             , created :: !Text
                             , updated :: !Text
                             , user :: !Text
                             , uri :: !Text
                             , text :: !Text
                             , tags :: ![Text]
                             , group :: !Text
                             , target :: ![Target]
                             , document :: !(Maybe Document)
                             , links :: !Links
                             } deriving (Show, Generic, Eq)

newtype Document = Document { title :: Maybe [Text] } deriving (Show, Generic, Eq)

data Links = Links { html :: !(Maybe Text)
                   , incontext :: !(Maybe Text)
                   , json :: !(Maybe Text)
                   } deriving (Show, Generic, Eq)

data Target = Target { source :: !(Maybe Text)
                     , selector :: !(Maybe [Selector])
                     } deriving (Show, Generic, Eq)

data SelectorType = RangeSelectorType
                  | TextPositionSelectorType
                  | TextQuoteSelectorType deriving (Show, Generic, Eq)

data Selector = TextQuoteSelector { _type :: !SelectorType
                                  , exact :: !Text
                                  , prefix :: !Text
                                  , suffix :: !Text
                                  }
              | TextPositionSelector { _type :: !SelectorType
                                     , start :: !Int
                                     , end :: !Int
                                     }
              | RangeSelector { _type :: !SelectorType
                              , startOffset :: !Int
                              , endOffset :: !Int
                              , startContainer :: !Text
                              , endContainer :: !Text
                              } deriving (Show, Generic, Eq)
