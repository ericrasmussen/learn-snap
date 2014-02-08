{-# LANGUAGE OverloadedStrings #-}

module Password
       ( passwordHandler
       , passwordSplices
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


-- a fake user type. note that actually storing passwords as text is a
-- terrible idea
data User = User {
    username :: Text
  , password :: Text
  } deriving Show

-- -----------------------------------------------------------------------------
-- * Provide a way to render a Maybe <your data type> as Text

maybeUserText :: Maybe User -> Text
maybeUserText Nothing  = "None"
maybeUserText (Just u) = T.append (username u) " is now logged in (not really)"

-- -----------------------------------------------------------------------------
-- * Optionally create predicates to validate form input

checkUsername :: Text -> Bool
checkUsername u = validLength u && T.all (\c -> c `elem` chars) u
  where chars       = ['A'..'z']
        validLength = (\l -> l > 3 && l < 26) . T.length

usernameError :: Text
usernameError = "Chars A-z only, 4-25 characters in length"

checkPassword :: Text -> Bool
checkPassword p = ((>4) . T.length) p && T.any isDigit p
  where isDigit d = d `elem` ['0'..'9']

passwordError :: Text
passwordError = "At least one number and more than 4 characters"

-- -----------------------------------------------------------------------------
-- * Define a form

-- creates a form with a single text input field
passwordForm :: Monad m => Form Text m User
passwordForm = User
  <$> "username" .: check usernameError checkUsername (text Nothing)
  <*> "password" .: check passwordError checkPassword (text Nothing)

-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

passwordSplices :: MonadSnap n => Splices (Splice n)
passwordSplices = makeFormSplices "password" passwordForm maybeUserText

-- -----------------------------------------------------------------------------
-- * Create a handler to render the Heist template

passwordHandler :: AppHandler ()
passwordHandler = cRender "/forms/password"

