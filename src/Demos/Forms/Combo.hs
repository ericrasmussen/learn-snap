{-# LANGUAGE OverloadedStrings #-}

module Demos.Forms.Combo
       ( comboHandler
       , comboSplices
       ) where

import Heist
import Heist.Compiled (Splice)
import Snap.Snaplet.Heist
import Snap.Core (MonadSnap)
import Text.Digestive
import Data.Text (Text)
import qualified Data.Text as T
import Application (AppHandler)
import Demos.Utils.Forms (makeFormSplices)


-- -----------------------------------------------------------------------------
-- * Define your data

-- tea choices
data Tea = Black | Green | White | Oolong | Puerh
  deriving (Eq, Show, Enum)

-- -----------------------------------------------------------------------------
-- * Provide a way to render your data type as Text

asText :: Tea -> Text
asText = T.pack . show

-- -----------------------------------------------------------------------------
-- * Define a form

-- create a form with a single select field listing Teas
form :: Monad m => Form Text m Tea
form = "combo" .: choice teaChoices Nothing

-- enumerate the tea choices for our form
teaChoices :: [(Tea, Text)]
teaChoices = map (\t -> (t, asText t)) [Black .. Puerh]

-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

comboSplices :: MonadSnap n => Splices (Splice n)
comboSplices = makeFormSplices "combo" "comboTabs" form asText

-- -----------------------------------------------------------------------------
-- * Create a handler to render the Heist template

comboHandler :: AppHandler ()
comboHandler = cRender "/forms/combo"

