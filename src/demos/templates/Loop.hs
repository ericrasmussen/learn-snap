{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- | This module demonstrates how you can repeat a section of a template by
-- mapping splice functions across a list of resources.
module Demos.Templates.Loop
  ( loopSplices
  , loopHandler
  ) where

------------------------------------------------------------------------------
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
import           Demos.Utils.Templates (makeTemplateSplices)
--------------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- * Define your data

data Color = Red | Green | Blue
  deriving (Show, Enum)


-- -----------------------------------------------------------------------------
-- * Provide functions to render your data as Text

toText :: Color -> Text
toText = T.pack . show

toHex :: Color -> Text
toHex Red   = "#C40233"
toHex Green = "#009F6B"
toHex Blue  = "#0087BD"

-- -----------------------------------------------------------------------------
-- * Functions for creating and working with RuntimeSplices

colorsRuntime :: Monad n => RuntimeSplice n [Color]
colorsRuntime = return [Red .. Blue]

renderColors :: Monad n => RuntimeSplice n [Color] -> C.Splice n
renderColors = C.manyWithSplices C.runChildren splicesFromColor

-- -----------------------------------------------------------------------------
-- * Create splices for our colors

-- this will bind a list of color splices (wrapped in a RuntimeSplice) to the
-- node <primaryColors/>
colorSplices :: Monad n => Splices (C.Splice n)
colorSplices = "primaryColors" ## renderColors colorsRuntime

-- this will bind the <colorName/> and <colorHex/> splices for a single color
splicesFromColor :: Monad n => Splices (RuntimeSplice n Color -> C.Splice n)
splicesFromColor = mapS (C.pureSplice . C.textSplice) $ do
  "colorName" ## toText
  "colorHex"  ## toHex


-- -----------------------------------------------------------------------------
-- * Create compiled Heist splices to export

-- takes the splices defined above and `mconcat`s them with display tab splices
loopSplices :: Monad m => Splices (C.Splice m)
loopSplices = mconcat [ colorSplices
                      , makeTemplateSplices "loop" "loopTabs"
                      ]

-- -----------------------------------------------------------------------------
-- * Create a handler to render the Heist template

loopHandler :: Handler App App ()
loopHandler = cRender "templates/loop/loop"
