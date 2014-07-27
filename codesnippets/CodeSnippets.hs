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
highlightGroup :: String -> String -> IO ()
highlightGroup path destination = do
  let tplName   = formatTplName path
  files         <- getSourceFiles path ".tpl"
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


-- preprocess all the relevant hs and tpl files
-- a potential TODO is creating a separate type for arguments to the two
-- highlighting functions, along the lines of:
-- data HighlightTask {
--   source :: FilePath
--   ext    :: Maybe String
--   dest   :: FilePath
--   group  :: Bool
-- }
-- Then we can simplify the logging part, avoid calling everything directly
-- in main, and possibly build up those structures from command line args or
-- a config file.
main = do
  putStrLn "creating tpl files from src/demos/forms/*.hs"
  highlightFiles "src/demos/forms" ".hs" "snaplets/heist/generated/code/forms"

  putStrLn "creating tpl files from snaplets/heist/forms/*.tpl"
  highlightFiles "snaplets/heist/forms" ".tpl" "snaplets/heist/generated/html/forms"

  putStrLn "creating tpl files from src/demos/templates/conditional.hs"
  highlightFiles "src/demos/templates" ".hs" "snaplets/heist/generated/code/templates"

  putStrLn "creating composite tpl file from snaplets/heist/templates/conditional"
  highlightGroup "snaplets/heist/templates/conditional" "snaplets/heist/generated/html/templates"

  putStrLn "creating composite tpl file from snaplets/heist/templates/multiple"
  highlightGroup "snaplets/heist/templates/multiple" "snaplets/heist/generated/html/templates"

  putStrLn "creating composite tpl file from snaplets/heist/templates/runtime"
  highlightGroup "snaplets/heist/templates/runtime" "snaplets/heist/generated/html/templates"

  putStrLn "creating composite tpl file from snaplets/heist/templates/loop"
  highlightGroup "snaplets/heist/templates/loop" "snaplets/heist/generated/html/templates"

