module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Model
import View
import Controller

main :: IO ()
main = playIO
    (InWindow "Asteroids" (900, 700) (100, 100))
    black
    60
    initialGameState
    (return . drawGame)
    handleInput
    updateWorld
