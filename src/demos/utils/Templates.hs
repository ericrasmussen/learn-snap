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

------------------------------------------------------------------------------

{-

Note: this API needs to be reconsidered. Currently it assumes that the form
module name (ex: TextInput.hs) and the corresponding form template name (ex:
textinput.tpl) will be the same after stripping the extension and converting
them to lowercase.

However, this is a reasonable assumption for now (it's an internal API and
will fail at load time if the templates are missing), but a bit fragile if
this continues to grow.

-}


-- reduces boilerplate for creating form and tab splices
-- assumes the form and tab splice names are unique (for now)


-- XXX: if heist demos are responsible for making their own splices (maybe? or
-- not) then this should really be makeTabSplices, and we need a way to pass in
-- references to the syntax highlighted code template too (right now it just
-- uses the html snippet)
-- but we also need to clean up these examples to fit this new format.
-- multiple templates fine; but only 2-3 max


makeTemplateSplices :: Monad m
                    => Text                   -- ^ node name/form identifier
                    -> Text                   -- ^ unique tabs identifier
                    -> Splices (C.Splice m)   -- ^ compiled splices to return
makeTemplateSplices node tabs = do
  tabs ## C.withLocalSplices (tabSplices node) noSplices (C.callTemplate "heist_tabs")


-- this is duplicated in utils/Forms.hs, so we should find a better home for it
data SnippetDir = Code | HTML


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
makeByteStringPath name dir = encodeUtf8 $ T.concat [base dir, T.toLower name]
  where base Code = "/generated/code/demos/"
        base HTML = "/generated/html/demos/"



-- creates a compiled Splice from a template
templateSplice :: Monad m => Text -> SnippetDir -> C.Splice m
templateSplice name dir = C.callTemplate $ makeByteStringPath name dir

