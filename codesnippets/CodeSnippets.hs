import Data.Char (toLower)
import Control.Applicative ((<$>))
import System.FilePath ( (</>)
                       , combine
                       , addExtension
                       , takeExtension
                       , replaceExtension
                       , splitDirectories
                       , takeFileName
                       )
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String

--------------------------------------------------------------------------------

-- filters a list of FilePaths by extension (ex: ".hs"; leading dot required)
filterExt :: String -> [FilePath] -> [FilePath]
filterExt ext = filter (\path -> takeExtension path == ext)


-- takes a file path relative to the current working directory and gets a list
-- of all the contents of that directory matching the supplied extension
getSourceFiles :: FilePath -> String -> IO [FilePath]
getSourceFiles path ext = do
  currentDir     <- getCurrentDirectory
  let workingDir = currentDir </> path
  filterExt ext <$> getDirectoryContents workingDir


-- choose "haskell" for .hs files and "xml" for .tpl files
langFromExt :: FilePath -> String
langFromExt path = case takeExtension path of
  ".hs"  -> "haskell"
  ".tpl" -> "xml"
  _      -> error "only for use with .tpl or .hs files"


-- takes the input folder, output folder, and file name (ex: Code.hs) and
-- then produces a syntax highlighted html fragment in the output folder, using
-- a tpl extension (ex: Code.tpl)
readAndWriteFile :: FilePath -> FilePath -> FilePath -> IO ()
readAndWriteFile inFolder outFolder name = do
  contents    <- readFile $ inFolder </> name
  -- lower case the file name before creating the output file path
  let newName = map toLower name
      outPath = outFolder </> (replaceExtension newName ".tpl")
      lang    = langFromExt name
  writeFile outPath $ highlight lang contents


-- highlight a given code block for a specified lang (haskell or xml)
highlight :: String -> String -> String
highlight lang code = renderHtml fragment
  where fragment = formatHtmlBlock defaultFormatOpts $ highlightAs lang code


-- highlights all files from path with the given extension, and outputs
-- the syntax highlighted html fragments as .tpl files in destination
highlightFiles :: String -> String -> String -> IO ()
highlightFiles path ext destination = do
  files <- getSourceFiles path ext
  mapM_ (readAndWriteFile path destination) files

-- finds all the tpl files in the given path, creates syntax highlighted
-- snippets from each, and creates one big snippet with all the parts delimited
-- by the individual tpl names. The resulting file will be placed in the
-- `destination` folder, and its name will be determined by the last folder in
-- `path`.
-- Example: highlightGroup "foo/bar" "foo/templates" will create a new file
-- foo/templates/bar.tpl that contains all of the syntax higlighted templates
-- from foo/bar/*.tpl.
highlightGroup :: String -> String -> String -> IO ()
highlightGroup path ext destination = do
  let tplName   = formatTplName path
  files         <- getSourceFiles path ext
  highlighted   <- mapM (highlightWithHeader tplName . combine path) files
  let composite =  concat highlighted
  let outFile   = destination </> (addExtension tplName ".tpl")
  writeFile outFile composite

highlightWithHeader :: String -> String -> IO String
highlightWithHeader dirName path = do
  contents <- readFile path
  let headerName  = takeFileName path
  let highlighted = highlight "xml" contents
  return $ addHeader headerName highlighted

-- kind of hacky way to add a header to an existing html snippet
addHeader :: String -> String -> String
addHeader name snippet = concat ["<h5>", name, "</h5>", snippet]

formatTplName :: FilePath -> String
formatTplName path = map toLower dirName
  where dirName = last . splitDirectories $ path



------------------------------------------------------------------------------
-- * All of the source/destination mappings for files to be highlighted

-- It's convenient to hardcode the paths for now (they're still all in one place
-- and easy to manage for as few as we have), but at some point we may want
-- to move these to a config file or have this programmer take the mappings
-- as command line args.
data HighlightTask = HT {
    src   :: FilePath
  , ext   :: String
  , dest  :: FilePath
  , group :: Bool
}

-- creates a HighlightTask record after adding the shared prefix to the dest dir
makeHT :: FilePath -> String -> FilePath -> Bool -> HighlightTask
makeHT s e d g = HT s e (prefixGenDir d) g

prefixTpl :: FilePath -> FilePath
prefixTpl path = "snaplets/heist/templates" </> path

prefixGenDir :: FilePath -> FilePath
prefixGenDir path = "snaplets/heist/generated" </> path

-- all the mappings we use currently
highlightMappings :: [HighlightTask]
highlightMappings = [
  -- source code for the form demos
    makeHT "src/Demos/Forms"            ".hs"  "code/forms"     False
  -- source code for the template demos
  , makeHT "src/Demos/Templates"        ".hs"  "code/templates" False
  -- source tpl for the form demos
  , makeHT "snaplets/heist/forms"       ".tpl" "html/forms"     False
  -- source tpl dirs for the composite template demos
  , makeHT (prefixTpl "conditional")    ".tpl" "html/templates" True
  , makeHT (prefixTpl "multiple")       ".tpl" "html/templates" True
  , makeHT (prefixTpl "runtime")        ".tpl" "html/templates" True
  , makeHT (prefixTpl "loop")           ".tpl" "html/templates" True
  ]

-- highlight files or groups
processTask :: HighlightTask -> IO ()
processTask ht = case group ht of
  False -> highlightFiles (src ht) (ext ht) (dest ht)
  True  -> highlightGroup (src ht) (ext ht) (dest ht)

-- really terrible ad hoc printing of highlight tasks, for use before processing
logTask :: HighlightTask -> IO ()
logTask ht = putStrLn $ concat [ "creating tpl files from "
                               , src ht
                               , "/*"
                               , ext ht
                               ]

-- preprocess all the relevant hs and tpl files
main = mapM_ (\h -> logTask h >> processTask h) highlightMappings
