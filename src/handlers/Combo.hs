{-# LANGUAGE OverloadedStrings #-}

module Combo
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
import Control.Applicative ((<$>), (<*>))
import Application (AppHandler)
import FormHelpers (makeFormSplices)


-- -----------------------------------------------------------------------------
-- * Define your data

-- tea choices
data Tea = Black | Green | White | Oolong | Puerh
  deriving (Eq, Show, Enum)

teaToText :: Tea -> Text
teaToText = T.pack . show

teaChoices :: [(Tea, Text)]
teaChoices = map (\t -> (t, teaToText t)) [Black .. Puerh]

-- -----------------------------------------------------------------------------
-- * Provide a way to render a Maybe <your data type> as Text

maybeTeaText :: Maybe Tea -> Text
maybeTeaText Nothing  = "None"
maybeTeaText (Just t) = teaToText t


-- -----------------------------------------------------------------------------
-- * Define a form

-- create a form with a single select field listing Teas
comboForm :: Monad m => Form Text m Tea
comboForm = "combo" .: choice teaChoices Nothing

-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

comboSplices :: MonadSnap n => Splices (Splice n)
comboSplices = makeFormSplices "combo" comboForm maybeTeaText

-- -----------------------------------------------------------------------------
-- * Create a handler to render the Heist template

comboHandler :: AppHandler ()
comboHandler = cRender "/forms/combo"

