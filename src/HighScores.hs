module HighScores where
import System.IO (readFile')
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (sort)

highScoreFile :: FilePath
highScoreFile = "highscores.txt"

loadHighScores :: IO [Int]
loadHighScores = do
	exists <- doesFileExist highScoreFile
	if exists 
		then do
			contents <- readFile' highScoreFile
			pure (mapMaybe readMaybe (lines contents))
		else pure []


saveHighScores :: [Int] -> IO ()
saveHighScores scores = writeFile highScoreFile (unlines(map show scores))

addHighScore :: Int -> [Int] -> [Int]
addHighScore s scores = take 5 (reverse (sort (s : scores)))