import System.Environment (getArgs)
import Data.List (isPrefixOf)

main = do
  [filename] <- getArgs
  playlog <- readFile filename
  template <- readFile "PlaylogViewerTemplate.html"
  mapM_ (putStrLn . replace playlog) (lines template)
  where
    replace s t | "<!--PLAYLOG-->"`isPrefixOf`t = s
                | otherwise = t
