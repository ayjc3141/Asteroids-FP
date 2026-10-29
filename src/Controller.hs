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


-- klaar
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey key keyState _ _) gs =                                                    -- eventkey = input event (toets indruk of loslaten), keystate = ingedrukt of niet, gs = gamestate, _ _ = muisinfo (onbelangrijk)
  case key of                                                                                   -- case of = welke toets is ingedrukt
    SpecialKey KeyLeft  -> pure $ updateInput (\i -> i { turnLeft  = isDown keyState }) gs      -- pijltje links, update turnLeft
    SpecialKey KeyRight -> pure $ updateInput (\i -> i { turnRight = isDown keyState }) gs      -- pijltje rechts, update turnRight
    SpecialKey KeyUp    -> pure $ updateInput (\i -> i { thrust    = isDown keyState }) gs      -- pijltje omhoog, update thrust
    SpecialKey KeySpace ->                                                                      -- pure is algemener dan return (applicative)
      case keyState of
        Down -> pure $ setShoot True  gs                                                        -- indrukken = schieten en start auto-fire
        Up   -> pure $ setShoot False gs                                                        -- loslaten = stop schieten
    Char 'p' | keyState == Down -> pure (togglePause gs)                                        -- p = pauze aan/uit
    _ -> pure gs                                                                                -- andere toets = niks doen
  where
    isDown Down = True                                                                          -- helperfunctie: toets ingedrukt
    isDown Up   = False                                                                         -- helperfunctie: toets losgelaten
handleInput _ gs = pure gs                                                                      -- onbekende input = niks doen


updateWorld :: Float -> GameState -> IO GameState
updateWorld dt gs                                                                               -- dt = tijd tussen frames, gs = huidige gamestate
  | paused gs = pure gs  -- als spel gepauzeerd = niks doen
  | lives gs <= 0 = pure gs
  | otherwise = do
      let inpRaw = playerInput (players gs)                                                                -- ruwe input van speler

      -- pas acties toe op volgorde
      let gs1 = if turnLeft  inpRaw then turnPlayer (-5) gs else gs                         -- draai links
      let gs2 = if turnRight inpRaw then turnPlayer 5 gs1 else gs1                          -- draai rechts
      let gs3 = if thrust    inpRaw then acceleratePlayer 1 gs2 else gs2                    -- thrust vooruit

      -- schieten en cooldown
      let gs4 = tryShoot dt gs3                                                                 -- altijd aanroepen, checkt cooldown en spawnt evt. bullet
      let gs5 = tryShootSaucer dt gs4
      -- beweeg alles
      let moved = gs5
                  { players = movePlayer dt (players gs5)
                  , enemies = map (moveEnemy dt) (enemies gs5)
                  , bullets = map (moveBullet dt) (bullets gs5)
                  , effects = updateAnimations dt (effects gs5)}
      let limited = limitPlayerSpeed moved                                                      -- limiet op speler snelheid

      let gs6 = handlePlayerCollisions limited

      let gs7 = handleBulletCollisions gs6
      let gs8 = handleSaucBulletCollisions gs7
      
      checkWave gs8                                                                  -- check of nieuwe wave nodig is


togglePause :: GameState -> GameState
togglePause gs = gs { paused = not (paused gs) }                                               -- toggle pauze aan/uit

updateInput :: (Input -> Input) -> GameState -> GameState
updateInput f gs =
  gs
      { players =
          p { playerInput = f (playerInput p) }                                                --update player input
      }
  where
    p = players gs

