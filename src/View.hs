module View where
import Graphics.Gloss
import Model

-- Teken gamestate naar picture, zelfde wereld geeft altijd dezelfde tekening (pure)
drawGame :: GameState -> Picture
drawGame gs =
  Pictures
    [ drawPlayers (players gs)
    , drawEnemies (enemies gs)
    , drawBullets (bullets gs)
    , drawEffects (effects gs)
    , drawHUD gs
    , drawPauseOverlay gs
    , drawGameOver gs
    ]

--Teken Game Over screen
drawGameOver :: GameState -> Picture
drawGameOver gs | lives gs <= 0 = Pictures [color (greyN 0.5) (rectangleSolid 900 700)
                                            , translate (-180) 100 $
                                              scale  0.5 0.5 $
                                              color red $
                                              Text "Game Over" 
                                            , translate (-100) 25 $
                                              scale 0.25 0.25 $
                                              color white $
                                              Text ("Score: " ++ show (score gs))
                                            , translate 0 (-100) $
                                              color white $
                                              rectangleSolid 200 50
                                            , translate (-70) (-110) $
                                              scale 0.25 0.25 $
                                              color black $
                                              Text "Opnieuw"
                                            ]
                | otherwise = Blank 

--Teken een player
drawPlayer :: Player -> Picture
drawPlayer p 
  | not (even (floor $ 10*iframes)) = Blank --Laat player knipperen als immuun
  | otherwise =
    translate x y $
    rotate rotDeg $
    color playerColor $
    polygon [(-10, -10), (20, 0), (-10, 10)]
  where
    iframes = playerIframes p
    Movement (x, y) _ rotDeg = playerMovement p
    playerColor | playerNr p == 1 = green
                | otherwise = blue

--Teken beide spelers
drawPlayers :: [Player] -> Picture 
drawPlayers ps = pictures (map drawPlayer ps)       


--Tekent de individuele lijnen bij een effect
drawParticle :: (Point, Point) -> Picture
drawParticle ((x, y), (vx, vy)) =
    Line [(x, y), (x + vx * 0.1, y + vy * 0.1)]

--Tekent een effect
drawEffect :: Animation -> Picture
drawEffect animation =
    Pictures
        [ Color kleur (drawParticle particle) | particle <- particles animation]
        where
          kleur = effectColor animation

--Tekent alle effects op scherm
drawEffects :: [Animation] -> Picture
drawEffects animations = Pictures (map drawEffect animations)

--Tekent een enemy
drawEnemy :: Enemy -> Picture
drawEnemy e =
  translate x y $
  rotate rotDeg $
  color (colorFor (enemyType e)) $
  shapeFor (enemyType e) 
  where
    Movement (x, y) _ rotDeg = enemyMovement e

--Tekent alle enemies
drawEnemies :: [Enemy] -> Picture
drawEnemies l = Pictures (map drawEnemy l)

--Teken een bullet op het scherm. Rood = enemy, geel = ally
drawBullet :: Bullet -> Picture
drawBullet b =
  translate x y $
  color kleur $
  circleSolid 2
  where
    Movement (x, y) _ _ = bulletMovement b
    kleur               = if bulletType b == Foe then red else yellow

--Teken alle bullets
drawBullets :: [Bullet] -> Picture
drawBullets l = Pictures (map drawBullet l)

--Geeft elke enemy een kleur
colorFor :: EnemyType -> Color
colorFor ent = case ent of
  MeteorEnormous  -> greyN 0.7
  AsteroidLarge   -> greyN 0.6
  AsteroidMedium  -> greyN 0.5
  AsteroidSmall   -> greyN 0.4
  AsteroidMoving  -> yellow
  SaucerEnormous  -> red
  SaucerSmall     -> orange

--Geeft elke enemy een vorm
shapeFor :: EnemyType -> Picture
shapeFor MeteorEnormous =
  thickCircle 40 4
shapeFor AsteroidLarge =
  circle 30
shapeFor AsteroidMedium =
  circle 20
shapeFor AsteroidSmall =
  circle 12
shapeFor AsteroidMoving =
  circle 12
shapeFor SaucerEnormous =           -- nogmaals, pictures met lijst van vormen, combineert
  scale 1.5 1 $ pictures            -- scale rekt uit. 1.5 horizontaal, 1 verticaal
    [ translate 0 5 (circleSolid 8)
    , translate 0 0 (rectangleSolid 30 8)
    ]
shapeFor SaucerSmall =
  pictures
    [ translate 0 4 (circleSolid 6)
    , translate 0 0 (rectangleSolid 20 6)
    ]

-- HUD = score, levens, wave
drawHUD :: GameState -> Picture             -- volgensmij pure
drawHUD gs =
  translate (-400) 300 $                    -- positie HUD
  scale 0.1 0.1 $                           -- kleiner
  color white $
  Text $
    "Score: " ++ show (score gs)
    ++ "  Lives: " ++ show (lives gs)
    ++ "  Wave: "  ++ show (wave gs)

-- Pauze scherm
drawPauseOverlay :: GameState -> Picture
drawPauseOverlay gs
  | paused gs =                                                     -- paused == true
      Pictures                                                      -- combineert alles
        [ color yellow (rectangleSolid 900 700)                     -- rechthoek over scherm
        , translate (-150) 0            $                           -- alle info voor paused text, translate voor positie
          scale 0.3 0.3                 $
          color black                   $
          Text "PAUSED"
        ]
  | otherwise = Blank                                               -- paused == false
