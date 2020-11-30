{-# LANGUAGE OverloadedStrings #-}
module Hypothesis.Mustache ( AnnotationWrapper (..) ) where

import Hypothesis.Base ( Annotation (..) )
import qualified Hypothesis.Base as H

import Text.Mustache ( ToMustache, object, toMustache, (~>) )

newtype AnnotationWrapper = AnnotationWrapper { w :: Annotation }

instance ToMustache AnnotationWrapper where
  toMustache (AnnotationWrapper a) = object [ "id" ~> H.id a
                                            , "created" ~> H.created a
                                            ]