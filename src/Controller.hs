{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Controller where
import Graphics.Gloss.Interface.IO.Game
import Model
import Shooting
import Movement
import Spawning (checkWave) 
import Animations
import System.Random (randomRIO)
import HighScores (addHighScore, saveHighScores)

--Handelt Keyboard inputs
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
    | lives gs <= 0 && insideButton (mx, my) = pure initialGameState { highScores = highScores gs }
    | otherwise = pure gs

handleInput (EventKey key keyState _ _) gs =
  --Speler 1 inputs
  case key of
    SpecialKey KeyLeft  -> pure $ updateInput 1 (\i -> i { turnLeft  = isDown keyState }) gs
    SpecialKey KeyRight -> pure $ updateInput 1 (\i -> i { turnRight = isDown keyState }) gs
    SpecialKey KeyUp    -> pure $ updateInput 1 (\i -> i { thrust    = isDown keyState }) gs
    SpecialKey KeySpace -> pure $ setShoot 1 (isDown keyState) gs

  
  --Speler 2 inputs
    Char 'a' -> pure $ updateInput 2 (\i -> i { turnLeft = isDown keyState }) gs
    Char 'd' -> pure $ updateInput 2 (\i -> i { turnRight = isDown keyState }) gs
    Char 'w' -> pure $ updateInput 2 (\i -> i { thrust = isDown keyState }) gs
    Char 'c' -> pure $ setShoot 2 (isDown keyState) gs
  
  --Pauzeer
    Char 'p' | keyState == Down -> pure (togglePause gs)
    _ -> pure gs
  where
    isDown Down = True  -- helperfunctie: toets ingedrukt
    isDown Up   = False -- helperfunctie: toets losgelaten
handleInput _ gs = pure gs -- onbekende input = niks doen

insideButton :: Point -> Bool
insideButton (mx, my) =
    mx >= -100 && mx <= 100 && my >= -125 && my <= -75

-- Update de hele game state
updateWorld :: Float -> GameState -> IO GameState
updateWorld dt gs
  | paused gs = pure gs   -- als spel gepauzeerd = niks doen
  | lives gs <= 0 = pure gs
  | otherwise = do
      angle <- randomRIO (-25, 25)    --schiet angle van kleine saucers
          
          -- schieten en cooldown
      let gs1 = tryShootAll dt gs
          gs2 = tryShootSaucer dt angle gs1
          
          -- beweeg alles
          moved = gs2
                  { players = map (handleIframes dt . movePlayer dt . limitPlayerSpeed . updatePlayerMovement) (players gs2)
                  , enemies = map (\e -> moveEnemy dt (position $ playerMovement $ closestPlayer e (players gs)) e)  (enemies gs2)
                  , bullets = filter onScreen (map (moveBullet dt) (bullets gs2))
                  , effects = updateAnimations dt (effects gs2)} 
          
          -- CHeckt of iets wordt geraakt
          gs3 = handlePlayerCollisions moved
          gs4 = handleBulletCollisions gs3
          gs5 = handleSaucBulletCollisions gs4
      
      gs6 <- checkWave gs5 -- check of nieuwe wave nodig is

      if lives gs6 <= 0
        then do 
            let newScores = addHighScore (score gs6) (highScores gs6)
            saveHighScores newScores
            pure gs6 {highScores = newScores}
        else pure gs6

-- toggle pauze aan/uit
togglePause :: GameState -> GameState
togglePause gs = gs { paused = not (paused gs) }

--Update player input state based op welke speler
updateInput :: Int -> (Input -> Input) -> GameState -> GameState
updateInput nr f gs =
  gs {players = map updatePlayer (players gs)}
  where
    updatePlayer p
      | playerNr p == nr = p {playerInput = f (playerInput p)}
      | otherwise = p

