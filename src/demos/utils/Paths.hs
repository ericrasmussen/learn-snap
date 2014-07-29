{-# LANGUAGE OverloadedStrings #-}

module Demos.Utils.Paths
       ( SnippetDir (..)
       , getFormsPath
       , getTemplatesPath
       ) where

import Data.Text (Text)

------------------------------------------------------------------------------


-- allows us to disambiguate template directories containing syntax-highlighted
-- haskell files (Code) or syntax-highlighted html5 templates (HTML)
data SnippetDir = Code | HTML

-- allows Demos.Utils.Templates to get the base code/html template directories
-- for heist template demos, without needing to know the paths
getTemplatesPath :: SnippetDir -> Text
getTemplatesPath Code = "/generated/code/templates/"
getTemplatesPath HTML = "/generated/html/templates/"

-- allows Demos.Utils.Forms to get the base code/html template directories
-- for digestive-functors form demos, without needing to know the paths
getFormsPath :: SnippetDir -> Text
getFormsPath Code = "/generated/code/forms/"
getFormsPath HTML = "/generated/html/forms/"
