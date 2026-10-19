{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
module Controller where
import Graphics.Gloss.Interface.IO.Game
import Model

-- wat ik niet heb kunnen oplossen: 2 invoeren (voor/ links of voor/ rechts) + schieten ingedrukt


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
      let inpAllowed = limitInputForRules inpRaw                                                -- filter input volgens regels (wat mag tegelijk)

      -- pas acties toe op volgorde
      let gs1 = if turnLeft  inpAllowed then turnPlayer (-5) gs else gs                         -- draai links
      let gs2 = if turnRight inpAllowed then turnPlayer 5 gs1 else gs1                          -- draai rechts
      let gs3 = if thrust    inpAllowed then acceleratePlayer 1 gs2 else gs2                    -- thrust vooruit

      -- schieten en cooldown
      let gs4 = tryShoot dt gs3                                                                 -- altijd aanroepen, checkt cooldown en spawnt evt. bullet

      -- beweeg alles
      let moved = gs4 
                  { players = movePlayer dt (players gs4)
                  , enemies = map (moveEnemy dt) (enemies gs4)
                  , bullets = map (moveBullet dt) (bullets gs4)
                  }                       
      let limited = limitPlayerSpeed moved                                                      -- limiet op speler snelheid
      
      let gs6 = handlePlayerCollisions limited

      let gs7 = handleBulletCollisions gs6

      pure (checkWave gs7)                                                                  -- check of nieuwe wave nodig is


limitInputForRules :: Input -> Input
limitInputForRules inp = inp                                       


checkWave :: GameState -> GameState
checkWave gs
  | null (filter isAsteroid (map enemyType (enemies gs))) =                                                     --geen asteroid?
      spawnNewWave (wave gs + 1) gs                                                             --dan nieuwe wave
  | otherwise = gs                                                                              --anders niks doen


  -- klaar
isAsteroid :: EnemyType -> Bool
isAsteroid et =
  et `elem`                                                                                     -- check of entitytype een asteroid is
    [ MeteorEnormous
    , AsteroidLarge
    , AsteroidMedium
    , AsteroidSmall
    , AsteroidMoving
    ]

spawnNewWave :: Int -> GameState -> GameState
spawnNewWave n gs =
  let newAsteroids =
        [ Enemy AsteroidLarge (Movement (x,y) (vx, vy) 0) 1 True 30                        -- nieuwe asteroid entiteiten
        | i <- [1 .. (3 + n)]                                                                   -- aantal groeit per wave
        , let x  = fromIntegral ((i * 150) `mod` 600 - 300)                                     -- x positie verspreid
        , let y  = fromIntegral ((i * 230) `mod` 600 - 300)                                     -- y positie verspreid
        , let vx = fromIntegral (((i * 7) `mod` 8) - 4)                                         -- random-achtige snelheid x
        , let vy = fromIntegral (((i * 5) `mod` 8) - 4)                                         -- random-achtige snelheid y
        ]
  in gs { enemies = enemies gs ++ newAsteroids, wave = n }                                    -- voeg toe aan game


  -- klaar
moveMovement :: Float -> Movement -> Movement
moveMovement dt m =
  let (x, y) = position m
      (vx, vy) = velocity m
  in m
       { position = (x + vx * dt, y + vy * dt) }

movePlayer :: Float -> Player -> Player
movePlayer dt p =
  p { playerMovement =
        moveMovement dt (playerMovement p) }

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt e =
  e { enemyMovement =
        moveMovement dt (enemyMovement e) }

moveBullet :: Float -> Bullet -> Bullet
moveBullet dt b =
  b { bulletMovement =
        moveMovement dt (bulletMovement b) }


   -- klaar

turnPlayer :: Float -> GameState -> GameState
turnPlayer deg gs =
  let p = players gs
      m = playerMovement p
  in gs
       { players =
           p { playerMovement =
                 m { rotation = rotation m + deg }
             }
       }                                                                         

-- klaar

acceleratePlayer :: Float -> GameState -> GameState
acceleratePlayer amt gs =
  let p = players gs
      m = playerMovement p
      (vx, vy) = velocity m
      dir = -(rotation m * pi / 180)                                                --graden -> radialen
      newVel =
        (vx + cos dir * amt, vy + sin dir * amt)                                  -- nieuwe snelheid
  in gs
       { players =
           p { playerMovement =
                 m { velocity = newVel }
             }
       }



limitPlayerSpeed :: GameState -> GameState
limitPlayerSpeed gs = 
      let p = players gs
          m = playerMovement p
          (vx,vy) = velocity m  
          speed = len (vx,vy)
          maxSpeed = 60
          factor = maxSpeed / speed                                               
      in if speed > maxSpeed                                                      --if speed exceeds max  
         then 
            gs
              { players =
                  p {playerMovement =
                      m {velocity = factor `scaleVec` (vx, vy)}                   --scale velocity down
                    }
              }
          else gs



tryShoot :: Float -> GameState -> GameState
tryShoot dt gs =                                                                               -- check cooldown en vuur indien mogelijk
  let cd' = max 0 (cooldown gs - dt)                                                           -- aftellen cooldown
  in if shoot (playerInput (players gs)) && cd' <= 0
        then spawnBullet (gs { cooldown = 1.5 })                                                 -- cooldown klaar -> spawn bullet
        else gs { cooldown = cd' }                                                             -- anders alleen aftellen


setShoot :: Bool -> GameState -> GameState
setShoot val gs =                                                                              -- update shootstatus bij indrukken/loslaten
  let p     = players gs
      inp   = playerInput p
      newGS = gs 
              { players = 
                p {playerInput = 
                  inp { shoot = val }
                  }
              }
  in if val && cooldown gs <= 0                                                                -- als ingedrukt en cooldown 0
        then spawnBullet newGS { cooldown = 1.5 }                                              -- spawn bullet en reset cooldown
        else newGS

spawnBullet :: GameState -> GameState
spawnBullet gs =
  let p = players gs
      m = playerMovement p
      (x, y) = position m
      (vx, vy) = velocity m
      dir = -(rotation m * pi / 180)
      offset = 20

      bulletPosition =
        (x + cos dir * offset, y + sin dir * offset)

      bulletVelocity =
        (vx + cos dir * 40, vy + sin dir * 40)

      bulletMovement' =
        Movement bulletPosition bulletVelocity (rotation m)

      bullet =
        Bullet bulletMovement' Friend 2

  in gs { bullets = bullet : bullets gs }


collides :: Point -> Float -> Point -> Float -> Bool
collides (x1, y1) r1 (x2, y2) r2 = len ((x1 - x2), (y1 - y2)) < r1 + r2

hitsPlayer :: Player -> Enemy -> Bool
hitsPlayer p e = collides (px, py) (playerHitbox p) (ex, ey) (enemyHitbox e) 
            where
                m = playerMovement p
                n = enemyMovement e
                (px, py) = position m
                (ex, ey) = position n



handlePlayerCollisions :: GameState -> GameState
handlePlayerCollisions gs | null hit = gs
                          | otherwise = gs { enemies = rest, lives = lives gs -1}
                        where
                            p = players gs
                            hit = filter (hitsPlayer p) (enemies gs)
                            rest = filter (not . hitsPlayer p) (enemies gs)

togglePause :: GameState -> GameState
togglePause gs = gs { paused = not (paused gs) }                                               -- toggle pauze aan/uit

hitsBullet :: Bullet -> Enemy -> Bool
hitsBullet b e = collides (bx, by) (bulletHitbox b) (ex, ey) (enemyHitbox e)
    where
        m = bulletMovement b
        n = enemyMovement e
        (bx, by) = position m
        (ex, ey) = position n

handleBulletCollisions :: GameState -> GameState
handleBulletCollisions gs | null hit = gs
                          | otherwise = gs {enemies = rest, bullets = restb, score = score gs + 100}
                        where
                            bs = bullets gs 
                            es = enemies gs
                            isHit e = any (\b -> hitsBullet b e) bs
                            hit = filter isHit es
                            rest = filter (not . isHit) (enemies gs)
                            bulletsHit b = any (\e -> hitsBullet b e) es
                            restb = filter (not . bulletsHit) bs

updateInput :: (Input -> Input) -> GameState -> GameState
updateInput f gs = 
  gs
      { players =
          p { playerInput = f (playerInput p) }                                                --update player input
      }
  where
    p = players gs
