{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module FormHelpers
       ( extraDigestiveSplices
       , checkError
       , makeFormSplices
       ) where

import Data.Monoid
import Heist
import qualified Heist.Compiled as C
import Text.Digestive
import Text.Digestive.Snap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XmlHtml as X
import qualified Text.Digestive.Heist.Compiled as DF
import Text.Digestive.View (errors)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Control.Monad (mplus)
import Data.Function (on)
import Data.List (unionBy)
import Snap.Core (MonadSnap)
import Control.Monad.Trans.Class (lift)
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------------------------

-- reduces boilerplate for creating forms that share the <captured> node and
-- checkerror attribute splice
makeFormSplices :: (Monad m, MonadSnap m)
                => Text                   -- node name/form identifier
                -> Form Text m v          -- the form to process
                -> (Maybe v -> Text)      -- convert Maybe results to Text
                -> Splices (C.Splice m)   -- the compiled splices we return
makeFormSplices node form handleResult = do
  let processed = runForm node form
      view      = liftM fst processed
      res       = liftM snd processed
  node          ## DF.formSplice' extraDigestiveSplices attrSplices (lift view)
  "captured"    ## (C.pureSplice . C.textSplice) handleResult $ (lift res)


-- when digestiveSplices just aren't enough
extraDigestiveSplices :: (Monad m) => RuntimeSplice m (View Text) -> Splices (C.Splice m)
extraDigestiveSplices v = do
    "dfLabelError"  ## dfLabelError  v
    "dfSmallErrors" ## dfSmallErrors v


-- shared attribute splices for all our forms (currently only "checkerror")
attrSplices :: Monad m => RuntimeSplice m (View Text) -> Splices (AttrSplice m)
attrSplices getView = do
  "checkerror" ## checkErrorRefs getView


-- creates an attribute splice by checking the attribute's value (a field path
-- reference) against a list of all paths with errors
checkErrorRefs :: Monad m
               => RuntimeSplice m (View Text)    -- the form view
               -> Text                           -- the attribute value
               -> RuntimeSplice m [(Text, Text)] -- the splice to return
checkErrorRefs getView ref = do
  view <- getView
  let refs  = getErrorSet view
      attrs = if ref `Set.member` refs then [("class", "error")] else []
  return attrs


-- creates a Set containing all form field paths with errors
getErrorSet :: View v -> Set Text
getErrorSet = Set.fromList . concatMap fst . viewErrors




-- * everything below needs refactoring


-- messy test for a custom label splice
dfLabelError :: Monad m => RuntimeSplice m (View v) -> C.Splice m
dfLabelError getView = do
    node <- getParamNode
    let (ref, attrs) = getRefAttributes node Nothing
    runAttrs <- C.runAttributesRaw attrs
    return $ C.yieldRuntime $ do
        view <- getView
        let hasError = fieldHasError ref view
        attrs' <- runAttrs
        let ref'     = absoluteRef ref view
        let attrs''  = if hasError then addErrorClass attrs' else attrs'
        let allAttrs = addAttrs attrs'' [("for", ref')]
        let e        = makeElement "label" (X.childNodes node) $ allAttrs
        return $ X.renderHtmlFragment X.UTF8 e

-- messy test for a custom error list splice
dfSmallErrors :: Monad m => RuntimeSplice m (View T.Text) -> C.Splice m
dfSmallErrors getView = do
    node <- getParamNode
    let (ref, attrs) = getRefAttributes node Nothing
    runAttrs <- C.runAttributesRaw attrs
    return $ C.yieldRuntime $ do
        view <- getView
        attrs' <- runAttrs
        let es = errors ref view
        let errorText = T.intercalate ", " es
        let elem = makeElement "small" [X.TextNode errorText] attrs' -- RENAME
        if null es then return mempty else return (X.renderHtmlFragment X.UTF8 elem)


fieldHasError :: Text -> View v -> Bool
fieldHasError ref view = not . null $ errors ref view

maybeAddError :: (Text, Text) -> (Text, Text)
maybeAddError ("class", val) = ("class", T.append val " error")
maybeAddError attrs          = attrs

addErrorClass :: [(Text, Text)] -> [(Text, Text)]
addErrorClass = map maybeAddError


-- borrowed from digestive-functors source. can be reworked for what we need

makeElement :: T.Text -> [X.Node] -> [(T.Text, T.Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes

getRefAttributes :: X.Node
                 -> Maybe T.Text              -- ^ Optional default ref
                 -> (T.Text, [(T.Text, T.Text)])  -- ^ (Ref, other attrs)
getRefAttributes node defaultRef =
    case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as `mplus` defaultRef
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])


-- | Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)
