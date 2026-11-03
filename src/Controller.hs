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

-- klaar
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey key keyState _ _) gs =                                                    -- eventkey = input event (toets indruk of loslaten), keystate = ingedrukt of niet, gs = gamestate, _ _ = muisinfo (onbelangrijk)
  --Speler 1 inputs
  case key of                                                                                   -- case of = welke toets is ingedrukt
    SpecialKey KeyLeft  -> pure $ updateInput 1 (\i -> i { turnLeft  = isDown keyState }) gs      -- pijltje links, update turnLeft
    SpecialKey KeyRight -> pure $ updateInput 1 (\i -> i { turnRight = isDown keyState }) gs      -- pijltje rechts, update turnRight
    SpecialKey KeyUp    -> pure $ updateInput 1 (\i -> i { thrust    = isDown keyState }) gs      -- pijltje omhoog, update thrust
    SpecialKey KeySpace -> pure $ setShoot 1 (isDown keyState) gs                                 -- pure is algemener dan return (applicative)

  
  --Speler 2 inputs
    Char 'a' -> pure $ updateInput 2 (\i -> i { turnLeft = isDown keyState }) gs
    Char 'd' -> pure $ updateInput 2 (\i -> i { turnRight = isDown keyState }) gs
    Char 'w' -> pure $ updateInput 2 (\i -> i { thrust = isDown keyState }) gs
    Char 'c' -> pure $ setShoot 2 (isDown keyState) gs
    
    Char 'p' | keyState == Down -> pure (togglePause gs)                                        -- p = pauze aan/uit
    _ -> pure gs                                                                                -- andere toets = niks doen
  where
    isDown Down = True                                                                          -- helperfunctie: toets ingedrukt
    isDown Up   = False                                                                         -- helperfunctie: toets losgelaten
handleInput _ gs = pure gs                                                                      -- onbekende input = niks doen


updateWorld :: Float -> GameState -> IO GameState
updateWorld dt gs                                                                               -- dt = tijd tussen frames, gs = huidige gamestate
  | paused gs = pure gs                                                                         -- als spel gepauzeerd = niks doen
  | lives gs <= 0 = pure gs
  | otherwise = do
      angle <- randomRIO (-25, 25)                                                          --schiet angle van kleine saucers
      -- schieten en cooldown
      let gs1 = tryShootAll dt gs                                                                 -- altijd aanroepen, checkt cooldown en spawnt evt. bullet
          gs2 = tryShootSaucer dt angle gs1
      -- beweeg alles
          moved = gs2
                  { players = map (handleIframes dt . movePlayer dt . limitPlayerSpeed . updatePlayerMovement) (players gs2)
                  , enemies = map (\e -> moveEnemy dt (position $ playerMovement $ closestPlayer e (players gs)) e)  (enemies gs2)
                  , bullets = map (moveBullet dt) (bullets gs2)
                  , effects = updateAnimations dt (effects gs2)} 
          
          gs3 = handlePlayerCollisions moved
          gs4 = handleBulletCollisions gs3
          gs5 = handleSaucBulletCollisions gs4
      
      checkWave gs5                                                                  -- check of nieuwe wave nodig is


togglePause :: GameState -> GameState
togglePause gs = gs { paused = not (paused gs) }                                               -- toggle pauze aan/uit

updateInput :: Int -> (Input -> Input) -> GameState -> GameState
updateInput nr f gs =
  gs {players = map updatePlayer (players gs)}
  where
    updatePlayer p
      | playerNr p == nr = p {playerInput = f (playerInput p)}
      | otherwise = p

