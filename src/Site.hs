{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Heist
import qualified Heist.Compiled as C
import           Data.Monoid
import           Snap.Core (MonadSnap)

-- these imports are from our standalone modules in src/handlers
import           TextInput (textInputHandler, textInputSplices)
import           TextArea  (textAreaHandler, textAreaSplices)
import           Password  (passwordHandler, passwordSplices)
import           Combo     (comboHandler,   comboSplices)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", indexHandler)
         -- guides
         , ("/guides/heist", heistGuide)
         , ("/guides/snap",  snapGuide)
         , ("/guides/forms", formsGuide)
         -- forms
         , ("/textinput", textInputHandler)
         , ("/textarea",  textAreaHandler)
         , ("/password",  passwordHandler)
         , ("/combo",     comboHandler)
         -- static assets
         , ("assets", serveDirectory "assets")
         ]

------------------------------------------------------------------------------
-- | Compose all the compiled splices imported from the handler modules
allCompiledSplices :: MonadSnap n => Splices (C.Splice n)
allCompiledSplices = mconcat [ textInputSplices
                             , textAreaSplices
                             , passwordSplices
                             , comboSplices
                             ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A snap demo application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit ""
    addConfig h $ mempty { hcCompiledSplices = allCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    return $ App h s

--------------------------------------------------------------------------------
-- | Our glorious index page
indexHandler :: AppHandler ()
indexHandler = render "index"

-- | A quick overview of Heist
heistGuide :: AppHandler ()
heistGuide = render "/guides/heist"

-- | A quick overview of Snap
snapGuide :: AppHandler ()
snapGuide = render "/guides/snap"

-- | A quick overview of digestive-functors/forms in Snap
formsGuide :: AppHandler ()
formsGuide = render "/guides/forms"
