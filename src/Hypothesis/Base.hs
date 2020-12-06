{-# LANGUAGE DeriveGeneric #-}
module Hypothesis.Base ( Annotation (..), Document (..), Links (..)
                      , Target (..), Selector (..), SelectorType (..)
                      , fromResult ) where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Aeson ( Result ( Success, Error ) )

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

data SelectorType = RangeSelector | TextPositionSelector | TextQuoteSelector
                    deriving (Show, Generic, Eq)

data Selector = Selector { _type :: !SelectorType
                         , exact :: !Text
                         , prefix :: !Text
                         , suffix :: !Text }
               | NAS
                         deriving (Show, Generic, Eq)

-- this should go to an internal library
fromResult :: Result a -> a
fromResult (Success r) = r
fromResult (Error e) = error e
