{-# LANGUAGE OverloadedStrings #-}

-- Highlighting executable based on the highlighting-kate example from:
--   https://github.com/jgm/highlighting-kate/blob/master/extra/Highlight.hs

import Control.Applicative ((<$>))
import Text.Highlighting.Kate
import System.IO (hPutStrLn, stderr)
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.FilePath (takeFileName)
import Data.Maybe (listToMaybe)
import Data.Char (toLower)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath ((</>), takeExtension, dropExtension, addExtension)
import System.Directory (getDirectoryContents, getCurrentDirectory)

--------------------------------------------------------------------------------


-- filters a list of FilePaths by extension (ex: ".hs"; leading dot required)
filterExt :: String -> [FilePath] -> [FilePath]
filterExt ext = filter (\path -> takeExtension path == ext)


-- replaces a file's extension with .tpl, which is what we'll be using when
-- preparing our html syntax highlighted heist templates
replaceExtTpl :: FilePath -> FilePath
replaceExtTpl = addExtension "tpl" . dropExtension


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
  let newName = outFolder </> replaceExtTpl name
  writeFile newName $ highlight (langFromExt name) contents


-- highlight a given code block for a specified lang (haskell or xml)
highlight :: String -> String -> String
highlight lang code = renderHtml fragment
  where fragment = formatHtmlBlock defaultFormatOpts $ highlightAs lang code


main = do
  hsFiles  <- getSourceFiles "src/handlers" ".hs"
  mapM_ (readAndWriteFile "src/handlers" "snaplets/heist/templates/code") hsFiles
  --tplFiles <- getSourceFiles "snaplets/heist/templates/forms/"
  -- "snaplets/heist/templates/html"
