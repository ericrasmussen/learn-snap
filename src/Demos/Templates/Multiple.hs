{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Demos.Templates.Multiple
  ( multipleHandler
  , multipleSplices
  ) where

------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * Create compiled Heist splices to export

-- Since this example only shows off templates that call other templates (no
-- logic for manipulating the nodes), the only splices we need are for the
-- "Code" and "Template" tabs we have on each page.
multipleSplices :: Monad m => Splices (C.Splice m)
multipleSplices = makeTemplateSplices "multiple" "multipleTabs"


--------------------------------------------------------------------------------
-- * Create a handler to render the Heist template

multipleHandler :: Handler App App ()
multipleHandler = cRender "templates/multiple/multiple"


