{-# LANGUAGE OverloadedStrings #-}

module Demos.Templates.Conditional
  ( conditionalHandler
  , conditionalSplices
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Monoid
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
import           Data.Maybe (fromMaybe)
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- * Define your data

data Tutorial = Tutorial {
    title  :: T.Text
  , url    :: T.Text
  , author :: Maybe T.Text
  }

-- helper function to show the conditional case: having a credited author or not
maybeAuthor :: Tutorial -> T.Text
maybeAuthor = fromMaybe "no credited author" . author


-- create two sample Tutorials
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


-- -----------------------------------------------------------------------------
-- * Create splices for each the tutorials

-- Note: you do not need to create separate splices for each author, and you can
-- see examples of this in the Heist loops demo. We create separate splices for
-- each tutorial here explicitly to illustrate conditional author field.
splicesA :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
splicesA = mapS (C.pureSplice . C.textSplice) $ do
  "titleA"       ## title
  "urlA"         ## url
  "maybeAuthorA" ## maybeAuthor

splicesB :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
splicesB = mapS (C.pureSplice . C.textSplice) $ do
  "titleB"       ## title
  "urlB"         ## url
  "maybeAuthorB" ## maybeAuthor

-- combine all the splices we've defined here
tutorialSplices :: Monad n => Splices (C.Splice n)
tutorialSplices = applyS tutorialA splicesA `mappend` applyS tutorialB splicesB



-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

-- takes the splices defined above and `mconcat`s them with display tab splices
conditionalSplices :: Monad m => Splices (C.Splice m)
conditionalSplices = mconcat [ tutorialSplices
                             , makeTemplateSplices "conditional" "conditionalTabs"
                             ]

--------------------------------------------------------------------------------
-- * Create a handler to render the Heist template

conditionalHandler :: Handler App App ()
conditionalHandler = cRender "templates/conditional/conditional"




