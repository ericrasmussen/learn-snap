{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Demos.Utils.Templates
       ( makeTemplateSplices
       ) where

import Data.Monoid
import Heist
import qualified Heist.Compiled as C
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Demos.Utils.Paths (SnippetDir (..), getTemplatesPath)

------------------------------------------------------------------------------

-- TODO: decide if this function should be responsible for all the splices
-- used in a heist template demo, or only the tabs (as it is currently).
makeTemplateSplices :: Monad m
                    => Text                   -- ^ node name/form identifier
                    -> Text                   -- ^ unique tabs identifier
                    -> Splices (C.Splice m)   -- ^ compiled splices to return
makeTemplateSplices node tabs = do
  tabs ## C.withLocalSplices (tabSplices node) noSplices (C.callTemplate "heist_tabs")


-- creates local tab splices (the code/template tabs on each demo) for
-- use when rendering the tabs template
tabSplices :: Monad m
           => Text
           -> Splices (C.Splice m)
tabSplices tplName = do
  "demoCode"     ## templateSplice tplName Code
  "templateCode" ## templateSplice tplName HTML


-- creates a ByteString representing the relative path to the Heist template
makeByteStringPath :: Text -> SnippetDir -> ByteString
makeByteStringPath name dir = encodeUtf8 path
  where path = T.concat [getTemplatesPath dir, T.toLower name]


-- creates a compiled Splice from a template
templateSplice :: Monad m => Text -> SnippetDir -> C.Splice m
templateSplice name dir = C.callTemplate $ makeByteStringPath name dir

