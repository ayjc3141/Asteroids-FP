module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import View
import Controller
import HighScores (loadHighScores)

main :: IO ()
main = do
    scores <- loadHighScores


    playIO
        (InWindow "Asteroids" (900, 700) (100, 100))
        black
        60
        (initialGameState {highScores = scores})
        (return . drawGame)
        handleInput
        updateWorld
