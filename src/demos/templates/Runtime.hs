{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Demos.Templates.Runtime
  ( runtimeHandler
  , runtimeSplices
  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as LL
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- * Create a pretend service that we can query at runtime

-- pretend fizzbuzz service showing how you can use an IO function within
-- Heist.Compiled splices (we don't use actual IO here, but the type is
-- important to illustrate how you could query outside services in a real app)
fizzbuzzService :: Int -> IO (Maybe Text)
fizzbuzzService = return . maybeFizzy
  where maybeFizzy n | n `mod` 15 == 0 = Just "fizzbuzz"
                     | n `mod` 5  == 0 = Just "buzz"
                     | n `mod` 3  == 0 = Just "fizz"
                     | otherwise       = Nothing


-- -----------------------------------------------------------------------------
-- * Functions for creating and working with RuntimeSplices

fizzRuntimes :: (Monad n, MonadIO n) => RuntimeSplice n [Int]
fizzRuntimes = return $ [1..10]


splicesFromFizz :: (Monad n, MonadIO n) => Splices (RuntimeSplice n Int -> C.Splice n)
splicesFromFizz = do
  "fizzIndex"      ## (C.pureSplice . C.textSplice) (T.pack . show)
  "maybeFizzValue" ## runtimeValue

-- this shows how to create a single compiled splice from a runtime value
-- the high level steps are:
--  * create a promise to be filled in by a value at runtime
--  * create a builder to get the runtime value and query the service
--  * choose the nothing or just_value templates based on the value
runtimeValue :: (Monad n, MonadIO n) => RuntimeSplice n Int -> C.Splice n
runtimeValue runtime = do
  nothing     <- C.callTemplate "nothing"
  promise     <- LL.newEmptyPromise
  valueSplice <- getValueSplice (LL.getPromise promise)
  let builder = C.yieldRuntime $ do
        value   <- runtime
        isFizzy <- liftIO $ fizzbuzzService value
        case isFizzy of
          Nothing -> C.codeGen nothing
          Just v  -> do
            LL.putPromise promise v
            C.codeGen valueSplice
  return builder

-- -----------------------------------------------------------------------------
-- * Create splices for our fizzbuzz sequence

renderFizzes :: (Monad n, MonadIO n) => RuntimeSplice n [Int] -> C.Splice n
renderFizzes = C.manyWithSplices C.runChildren splicesFromFizz


fizzSplices :: (Monad n, MonadIO n) => Splices (C.Splice n)
fizzSplices = "fizzbuzzes" ## renderFizzes fizzRuntimes

getValueSplice :: (Monad n, MonadIO n) => RuntimeSplice n T.Text -> C.Splice n
getValueSplice = C.withSplices template local
  where template = C.callTemplate "just_value"
        local    = "value" ## C.pureSplice . C.textSplice $ id


--------------------------------------------------------------------------------
-- * Create compiled Heist splices to export

-- takes the splices defined above and `mconcat`s them with display tab splices
runtimeSplices :: (Monad n, MonadIO n) => Splices (C.Splice n)
runtimeSplices = mconcat [ fizzSplices
                         , makeTemplateSplices "runtime" "runtimeTabs"
                         ]

--------------------------------------------------------------------------------
-- * Create a handler to render the Heist template

runtimeHandler :: Handler App App ()
runtimeHandler = cRender "templates/runtime/runtime"
