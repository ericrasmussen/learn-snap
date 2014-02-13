{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module FormHelpers
       ( makeFormSplices
       ) where

import Data.Monoid
import Heist
import qualified Heist.Compiled as C
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
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

{-

Note: this API needs to be reconsidered. Currently it assumes that the form
module name (ex: TextInput.hs) and the corresponding form template name (ex:
textinput.tpl) will be the same after stripping the extension and converting
them to lowercase.

However, this is a reasonable assumption for now (it's an internal API and
will fail at load time if the templates are missing), but a bit fragile if
this continues to grow.

-}


-- reduces boilerplate for creating form and tab splices
-- assumes the form and tab splice names are unique (for now)
makeFormSplices :: (Monad m, MonadSnap m)
                => Text                   -- ^ node name/form identifier
                -> Text                   -- ^ unique tabs identifier
                -> Form Text m v          -- ^ the form to process
                -> (v -> Text)            -- ^ display results as Text
                -> Splices (C.Splice m)   -- ^ the compiled splices we return
makeFormSplices node tabs form toText = do
  let processed  = runForm node form
      view       = liftM fst processed
      res        = liftM snd processed
  node ## DF.formSplice' extraDigestiveSplices attrSplices (lift view)
  tabs ## C.withLocalSplices (tabSplices node res toText) noSplices (C.callTemplate "tabs")


-- creates local tab splices (the result/code/template tabs on each demo) for
-- use when rendering the tabs template
tabSplices :: (Monad m, MonadSnap m)
           => Text
           -> m (Maybe v)
           -> (v -> Text)
           -> Splices (C.Splice m)
tabSplices tplName res asText = do
  "captured"     ## (C.pureSplice . C.textSplice) maybeText $ lift res
  "formCode"     ## templateSplice tplName Code
  "templateCode" ## templateSplice tplName HTML
    where
      maybeText = fromMaybe "None" . fmap asText


-- to avoid passing around Strings for the folder locations
data SnippetDir = Code | HTML


-- creates a ByteString representing the relative path to the Heist template
makeByteStringPath :: Text -> SnippetDir -> ByteString
makeByteStringPath name dir = encodeUtf8 $ T.concat [base dir, T.toLower name]
  where base Code = "/code/"
        base HTML = "/html/"


-- creates a compiled Splice from a template
templateSplice :: (Monad m, MonadSnap m) => Text -> SnippetDir -> C.Splice m
templateSplice name dir = C.callTemplate $ makeByteStringPath name dir


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


-- similar to the dfLabel splice from digestive-functors, but will conditionally
-- add an error class if the referenced node has an error in the form view
dfLabelError :: Monad m => RuntimeSplice m (View v) -> C.Splice m
dfLabelError getView = do
    node <- getParamNode
    let ref = getRef node
    return $ C.yieldRuntime $ do
        view <- getView
        let style = if fieldHasError ref view then "inline error" else "inline"
            attrs = [("for", absoluteRef ref view), ("class", style)]
            label = X.Element "label" attrs (X.childNodes node)
        return $ X.renderHtmlFragment X.UTF8 [label]


-- similar to the dfErrorList splice from digestive-functors, but renders the
-- error list as comma separted text in a <small> tag
dfErrorsInline :: Monad m => RuntimeSplice m (View T.Text) -> C.Splice m
dfErrorsInline getView = do
    node <- getParamNode
    let ref = getRef node
    return $ C.yieldRuntime $ do
        view   <- getView
        let errorList = errors ref view
            errorText = T.intercalate ", " errorList
            attrs     = [("class", "error")]
            small     = X.Element "small" attrs [X.TextNode errorText]
        -- only render the element if we have one or more errors
        case errorList of
          [] -> return mempty
          _  -> return (X.renderHtmlFragment X.UTF8 [small])


-- partial function to lookup the value of a given node's "ref" attribute.
-- if that value is not supplied then we cannot render the template.
-- (the use of error here is unfortunate, but consistent with
-- digestive-functor's own internal Heist usage)
getRef :: X.Node -> T.Text
getRef node = case node of
  (X.Element _ attrs _) -> fromMaybe showError $ lookup "ref" attrs
    where showError = error $ "missing ref in node: " ++ show node
  _                  -> error "Wrong type of node!"

