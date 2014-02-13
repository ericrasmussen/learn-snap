{-# LANGUAGE OverloadedStrings #-}

module TextInput
       ( textInputHandler
       , textInputSplices
       ) where

import Heist
import Heist.Compiled (Splice)
import Snap.Snaplet.Heist
import Snap.Core (MonadSnap)
import Text.Digestive
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Application (AppHandler)
import FormHelpers (makeFormSplices)


-- -----------------------------------------------------------------------------
-- * Define your data

data SomeText = SomeText Text
  deriving Show

-- -----------------------------------------------------------------------------
-- * Provide a way to render a Maybe <your data type> as Text

asText :: SomeText -> Text
asText = T.pack . show

-- -----------------------------------------------------------------------------
-- * Optionally create predicates to validate form input

-- check that the input Text is non-empty
checkText :: Text -> Bool
checkText "" = False
checkText _  = True

-- -----------------------------------------------------------------------------
-- * Define a form

-- creates a form with a single text input field
form :: Monad m => Form Text m SomeText
form = SomeText
  <$> "textinput" .: check "Must not be empty" checkText (text Nothing)

-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

textInputSplices :: MonadSnap n => Splices (Splice n)
textInputSplices = makeFormSplices "textInput" "textInputTabs" form asText

-- -----------------------------------------------------------------------------
-- * Create a handler to render the Heist template

textInputHandler :: AppHandler ()
textInputHandler = cRender "/forms/textinput"

