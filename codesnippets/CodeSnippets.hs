import Data.Char (toLower)
import Control.Applicative ((<$>))
import System.FilePath ( (</>)
                       , combine
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
-- by the individual tpl names
highlightGroup :: String -> String -> IO ()
highlightGroup path destination = do
  let tplName   = formatTplName path
  files         <- getSourceFiles path ".tpl"
  highlighted   <- mapM (highlightWithHeader tplName . combine path) files
  let composite =  concat highlighted
  writeFile destination composite

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
main = do
  putStrLn "creating tpl files from src/demos/forms/*.hs"
  highlightFiles "src/demos/forms" ".hs" "snaplets/heist/generated/code"

  putStrLn "creating tpl files from snaplets/heist/forms/*.tpl"
  highlightFiles "snaplets/heist/forms" ".tpl" "snaplets/heist/generated/html"

  putStrLn "creating tpl files from src/demos/compiled/conditional/*.hs"
  highlightFiles "src/demos/compiled/conditional" ".hs" "snaplets/heist/generated/code/demos"

  putStrLn "creating composite tpl file from snaplets/heist/conditional/text"
  highlightGroup "snaplets/heist/conditional/text" "snaplets/heist/generated/html/demos/text.tpl"
