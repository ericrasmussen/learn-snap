import Data.Char (toLower)
import Control.Applicative ((<$>))
import System.FilePath ((</>), takeExtension, replaceExtension)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String

--------------------------------------------------------------------------------


-- filters a list of FilePaths by extension (ex: ".hs"; leading dot required)
filterExt :: String -> [FilePath] -> [FilePath]
filterExt ext = filter (\path -> takeExtension path == ext)


-- replaces a file's extension with .tpl, which is what we'll be using when
-- takes a file path relative to the current working directory and gets a list
-- of all the contents of that directory with the supplied extension
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

-- preprocess all the relevant hs and tpl files
main = do
  putStrLn "creating tpl files from src/handlers/*.hs"
  highlightFiles "src/handlers" ".hs" "snaplets/heist/code"
  putStrLn "...new .tpl files now available in snaplets/heist/code"

  -- extra line space to make output cleaner
  putStrLn ""

  putStrLn "creating tpl files from snaplets/heist/forms/*.tpl"
  highlightFiles "snaplets/heist/forms" ".tpl" "snaplets/heist/html"
  putStrLn "...new .tpl files now available in snaplets/heist/html"

