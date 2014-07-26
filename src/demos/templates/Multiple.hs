{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The purpose of this module is to show off various ways to conditionally
-- include content in Heist templates. You will see some copy/paste and shared
-- code here in other modules, but the intent is to make this a standalone
-- module so you don't need to follow a bunch of imports in order to understand
-- these examples.

module Demos.Templates.Multiple
  ( multipleHandler
  , multipleSplices
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as LL
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------

-- simple data type for Tutorials that may or may not have an author
data Tutorial = Tutorial {
    title  :: T.Text
  , url    :: T.Text
  , author :: Maybe T.Text
  }

-- unlike the interpreted version, we want values of Tutorial elevated to
-- RuntimeSplice n Tutorial so they can be passed in to compiled splices. We
-- could use this same pattern for values pulled from a database or other source
tutorialA :: Monad n => RuntimeSplice n Tutorial
tutorialA = return Tutorial {
    title  = "Heist Template Tutorial"
  , url    = "http://snapframework.com/docs/tutorials/heist"
  , author = Nothing
  }

tutorialB :: Monad n => RuntimeSplice n Tutorial
tutorialB = return Tutorial {
    title  = "Looping and Control Flow in Heist"
  , url    = "http://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html"
  , author = Just "mightybyte"
  }


-- helper function to show the conditional case: having a credited author or not
maybeAuthor :: Tutorial -> T.Text
maybeAuthor = fromMaybe "no credited author" . author

--------------------------------------------------------------------------------
-- * A handler to demonstrate conditionally rendering an entire template and
-- attaching it to a node.

-- | Similar to conditionalHandler, except it conditionally inserts a rendered
-- template instead of Text
multipleHandler :: Handler App App ()
multipleHandler = cRender "templates/multiple/multiple"

-- | Top level splices that will be bound to an empty value or a fully rendered
-- template with local splices. We pass in a Maybe (RuntimeSplice n Tutorial) to
-- authorSplices to make it easier to decide whether or not to render the
-- template, because there is no easy way to directly inspect the return value
-- of a RuntimeSplice computation.
allAuthorSplices :: Monad n => Splices (C.Splice n)
allAuthorSplices = do
  "authorA" ## authorSplices Nothing
  "authorB" ## authorSplices (Just tutorialB)

-- | Takes a Maybe RuntimeSplice and either returns nothing (a splice created
-- from an empty node list) or returns a template splice using local
-- "authorName" splices
authorSplices :: Monad n => Maybe (RuntimeSplice n Tutorial) -> C.Splice n
authorSplices Nothing        = C.runNodeList []
authorSplices (Just runtime) = C.withSplices authorTemplateSplice local runtime
    where local = "authorName" ## (C.pureSplice . C.textSplice $ maybeAuthor)

-- | Renders the authorinfo template. Intended to be used with withSplices.
authorTemplateSplice :: Monad n => C.Splice n
authorTemplateSplice = C.callTemplate "authorinfo"

multipleSplices :: Monad m => Splices (C.Splice m)
multipleSplices = mconcat [ allAuthorSplices
                          , makeTemplateSplices "multiple" "multipleTabs"
                          ]
