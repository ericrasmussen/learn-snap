{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The purpose of this module is to show off various ways to conditionally
-- include content in Heist templates. You will see some copy/paste and shared
-- code here in other modules, but the intent is to make this a standalone
-- module so you don't need to follow a bunch of imports in order to understand
-- these examples.

module Demos.Templates.Runtime
  ( runtimeHandler
  , runtimeSplices
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
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

--------------------------------------------------------------------------------
-- * Similar the conditional template handler, but showing how to inspect a
-- runtime value.

-- | Elevating Maybe Text values to RuntimeSplice values
runtimeA :: Monad n => RuntimeSplice n (Maybe T.Text)
runtimeA = return $ Just "a value"

runtimeB :: Monad n => RuntimeSplice n (Maybe T.Text)
runtimeB = return Nothing

-- | Renders the template using any splices in our HeistConfig
runtimeHandler :: Handler App App ()
runtimeHandler = cRender "templates/runtime/runtime"

-- | The top level splices we export to our HeistConfig
allRuntimeValueSplices :: Monad n => Splices (C.Splice n)
allRuntimeValueSplices = do
  "runtimeA" ## runtimeValue runtimeA
  "runtimeB" ## runtimeValue runtimeB

-- | Inspects a runtime value in order to decide which template to render
runtimeValue :: Monad n => RuntimeSplice n (Maybe T.Text) -> C.Splice n
runtimeValue runtime = do
  -- a compiled splice for a static template (will be used in the Nothing case)
  nothing     <- C.callTemplate "nothing"

  -- create a new empty promise that can be filled in with a Text value
  promise     <- LL.newEmptyPromise

  -- instead of passing the RuntimeSplice directly to valueSplice, we pass in
  -- a function capable of getting the value out of a promise
  valueSplice <- getValueSplice (LL.getPromise promise)

  -- The 'do' block below has a value of type:
  --   RuntimeSplice n Builder -> DList (Chunk n)
  --
  -- This lets us extract the underlying value (Maybe Text) from the runtime
  let builder = C.yieldRuntime $ do
        value <- runtime
        case value of
          -- in the Nothing case we convert the template splice to a builder
          Nothing -> C.codeGen nothing
          -- in the Just case we put the extracted Text value in a promise
          -- first, then convert the compiled value splice to a builder
          Just v  -> do
            LL.putPromise promise v
            C.codeGen valueSplice

  -- our builder has the type DList (Chunk n), and remember that:
  --   type Splice n = HeistT n IO (DList (Chunk n))
  -- so returning this value to the current monad finally creates the fully
  -- compiled splice we needed
  return builder

-- | Note that here we take a runtime Text value, *not* Maybe Text. At this
-- point we know we have a value so we can bind it to a local splice and call
-- a template expecting a <value/> node
getValueSplice :: Monad n => RuntimeSplice n T.Text -> C.Splice n
getValueSplice = C.withSplices template local
  where template = C.callTemplate "just_value"
        local    = "value" ## C.pureSplice . C.textSplice $ id

-- takes the splices defined above and `mconcat`s them with display tab splices
runtimeSplices :: Monad m => Splices (C.Splice m)
runtimeSplices = mconcat [ allRuntimeValueSplices
                         , makeTemplateSplices "runtime" "runtimeTabs"
                         ]
