{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module TextInput
       ( textInputHandler
       , textInputSplices
       ) where


import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Control.Lens.TH
import Data.Text (Text)
import Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as LL
import Snap.Http.Server (defaultConfig, httpServe)
import Snap.Snaplet
import Snap.Snaplet.Heist
import System.IO (hPutStrLn, stderr)
import Text.Digestive
import Text.Digestive.Heist
import qualified Text.Digestive.Heist.Compiled as DF
import Text.Digestive.Snap
import qualified Data.Text as T
import Data.Monoid
import Snap.Types (MonadSnap)
import Control.Monad (liftM, join)
import Control.Monad.Trans.Class (lift)
import Application

-- for trying to create error utility functions
import qualified Text.XmlHtml as X
import Data.Monoid
import Data.Maybe (fromMaybe)
import           Control.Monad            (mplus)
import           Data.Function            (on)
import           Data.List                (unionBy)

import Text.Digestive.View (errors, childErrors)

import FormHelpers

------------------------------------------------------------------------------

-- define a data type
data SomeText = SomeText Text
  deriving Show

-- define a way to render a maybe result as Text
maybeSomeText :: Maybe SomeText -> Text
maybeSomeText Nothing  = "None"
maybeSomeText (Just t) = T.pack . show $ t

-- optionally create predicates to check results
checkText :: Text -> Bool
checkText "" = False
checkText _  = True

-- define the form
textInputForm :: Monad m => Form Text m SomeText
textInputForm = SomeText
  <$> "textinput" .: check "Must not be empty" checkText (text Nothing)

-- create a handler to render the template
textInputHandler :: Handler App App ()
textInputHandler = cRender "forms/textinput"

-- create the splices to export
textInputSplices :: MonadSnap n => Splices (C.Splice n)
textInputSplices = makeFormSplices "textInputForm" textInputForm maybeSomeText
