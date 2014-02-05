{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module FormHelpers
       ( makeFormSplices
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
    "dfLabelError"   ## dfLabelError  v
    "dfErrorsInline" ## dfErrorsInline v


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


-- checks if a form view has an error for the supplied field ref
fieldHasError :: Text -> View v -> Bool
fieldHasError ref = not . null . errors ref




-- * everything below needs refactoring


-- messy test for a custom label splice
dfLabelError :: Monad m => RuntimeSplice m (View v) -> C.Splice m
dfLabelError getView = do
    node <- getParamNode
    let ref = getRef node
    return $ C.yieldRuntime $ do
        view <- getView
        let ref'   = absoluteRef ref view
            style  = if fieldHasError ref view then "inline error" else "inline"
            attrs' = [("for", ref'), ("class", style)]
            label  = X.Element "label" attrs' (X.childNodes node)
        return $ X.renderHtmlFragment X.UTF8 [label]


-- messy test for a custom error list splice
dfErrorsInline :: Monad m => RuntimeSplice m (View T.Text) -> C.Splice m
dfErrorsInline getView = do
    node <- getParamNode
    let ref = getRef node
    return $ C.yieldRuntime $ do
        view   <- getView
        let es        = errors ref view
            errorText = T.intercalate ", " es
            small     = X.Element "small" [("class", "error")] [X.TextNode errorText]
        if null es then return mempty else return (X.renderHtmlFragment X.UTF8 [small])


-- partial function to lookup the value of a given node's "ref" attribute.
-- if that value is not supplied then we cannot render the template.
-- (the use of error here is unfortunate, but consistent with
-- digestive-functor's own internal Heist usage)
getRef :: X.Node -> T.Text
getRef node = case node of
  (X.Element _ attrs _) -> fromMaybe showError $ lookup "ref" attrs
    where showError = error $ "missing ref in node: " ++ show node
  _                  -> error "Wrong type of node!"

