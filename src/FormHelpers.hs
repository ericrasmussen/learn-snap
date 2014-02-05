{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module FormHelpers
       ( extraDigestiveSplices
       , checkError
       , makeFormSplices
       ) where

import           Data.Monoid
import           Data.Text (Text)
import           Heist
import           Text.Digestive
import           Text.Digestive.Snap
import qualified Data.Text      as T
import qualified Text.XmlHtml   as X
import qualified Heist.Compiled as C
import qualified Text.Digestive.Heist.Compiled as DF
import           Control.Monad             (liftM)
import           Text.Digestive.View       (errors)
import           Data.Maybe                (fromMaybe)
import           Control.Monad             (mplus)
import           Data.Function             (on)
import           Data.List                 (unionBy)
import           Snap.Core                 (MonadSnap)
import           Control.Monad.Trans.Class (lift)

------------------------------------------------------------------------------

-- api could use some work.
makeFormSplices :: (Monad m, MonadSnap m)
                => Text
                -> Form Text m v
                -> (Maybe v -> Text)
                -> Splices (C.Splice m)
makeFormSplices node form handleResult = do
  let processed = runForm node form
  let view      = liftM fst processed
  let res       = liftM snd processed
  node          ## DF.formSplice' extraDigestiveSplices checkError (lift view)
  "captured"    ## (C.pureSplice . C.textSplice) handleResult $ (lift res)


checkErrorRefs :: Monad m => RuntimeSplice m (View Text) -> Text -> RuntimeSplice m [(Text, Text)]
checkErrorRefs getView ref = do
  view <- getView
  let errors = getErrorRefs view  -- RENAME!!
  let attrs = if ref `elem` errors then [("class", "error")] else []
  return attrs

-- rename to reflect multiple attr splices like defaultAttrSplices
checkError :: Monad m => RuntimeSplice m (View Text) -> Splices (AttrSplice m)
checkError getView = do
  "checkerror" ## checkErrorRefs getView

-- lists all the paths with errors
getErrorRefs :: View v -> [Text]
getErrorRefs = concatMap fst . viewErrors

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

extraDigestiveSplices :: (Monad m) => RuntimeSplice m (View Text) -> Splices (C.Splice m)
extraDigestiveSplices v = do
    "dfLabelError"  ## dfLabelError  v
    "dfSmallErrors" ## dfSmallErrors v


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
