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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Control.Monad.IO.Class (liftIO)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as LL
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------


data Fizzbuzz = Fizzbuzz {
    index      :: Int
  , maybeFizzy :: Maybe Text
}

fizzIntText :: Fizzbuzz -> Text
fizzIntText = T.pack . show . index

fizzbuzzService :: Int -> IO (Maybe Text)
fizzbuzzService = return . maybeFizzy
  where maybeFizzy n | n `mod` 15 == 0 = Just "fizzbuzz"
                     | n `mod` 5  == 0 = Just "buzz"
                     | n `mod` 3  == 0 = Just "fizz"
                     | otherwise       = Nothing


-- fun TODO: make a fake fizzbuzz service instead to show how you can add IO
-- to the type when deciding what to do with a runtime value
fizzbuzzes :: [Fizzbuzz]
fizzbuzzes = map (\x -> Fizzbuzz x  (mkFizzbuzz x)) [1..]
  where mkFizzbuzz n | n `mod` 15 == 0 = Just "fizzbuzz"
                     | n `mod` 5  == 0 = Just "buzz"
                     | n `mod` 3  == 0 = Just "fizz"
                     | otherwise       = Nothing

fizzRuntimes :: Monad n => RuntimeSplice n [Fizzbuzz]
fizzRuntimes = return $ take 10 fizzbuzzes

renderFizzes :: Monad n => RuntimeSplice n [Fizzbuzz] -> C.Splice n
renderFizzes = C.manyWithSplices C.runChildren splicesFromFizz


fizzSplices :: Monad n => Splices (C.Splice n)
fizzSplices = "fizzbuzzes" ## renderFizzes fizzRuntimes

splicesFromFizz :: Monad n => Splices (RuntimeSplice n Fizzbuzz -> C.Splice n)
splicesFromFizz = do
  "fizzIndex"      ## (C.pureSplice . C.textSplice) fizzIntText
  "maybeFizzValue" ## runtimeValue

runtimeValue :: Monad n => RuntimeSplice n Fizzbuzz -> C.Splice n
runtimeValue runtime = do
  nothing     <- C.callTemplate "nothing"
  promise     <- LL.newEmptyPromise
  valueSplice <- getValueSplice (LL.getPromise promise)
  let builder = C.yieldRuntime $ do
        value <- runtime
        --isFizzy <- liftIO $ fizzbuzzService (index value)
        case maybeFizzy value of
        --case isFizzy of
          Nothing -> C.codeGen nothing
          Just v  -> do
            LL.putPromise promise v
            C.codeGen valueSplice
  return builder

getValueSplice :: Monad n => RuntimeSplice n T.Text -> C.Splice n
getValueSplice = C.withSplices template local
  where template = C.callTemplate "just_value"
        local    = "value" ## C.pureSplice . C.textSplice $ id

-- takes the splices defined above and `mconcat`s them with display tab splices
runtimeSplices :: Monad m => Splices (C.Splice m)
runtimeSplices = mconcat [ fizzSplices
                         , makeTemplateSplices "runtime" "runtimeTabs"
                         ]


-- | Renders the template using any splices in our HeistConfig
runtimeHandler :: Handler App App ()
runtimeHandler = cRender "templates/runtime/runtime"
