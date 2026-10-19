module View where

import Graphics.Gloss
import Model

-- ik weet niet of de types allemaal kloppen, maar zo leek het enigzins logisch.


-- teken gamestate naar picture, zelfde wereld geeft altijd dezelfde tekening (pure)
drawGame :: GameState -> Picture
drawGame gs =
  Pictures
    [ drawPlayer (players gs)
    , drawEnemies (enemies gs)
    , drawBullets (bullets gs)
    , drawEffects (effects gs)
    , drawHUD gs
    , drawPauseOverlay gs
    , drawGameOver gs
    ]

drawGameOver :: GameState -> Picture
drawGameOver gs | lives gs <= 0 = Pictures [color (greyN 0.5) (rectangleSolid 900 700)
                                            , translate (-200) 100 $
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
                                            , translate 0 (-100) $
                                              scale 0.25 0.25 $
                                              color black $
                                              Text "Opnieuw"
                                            ]
                | otherwise = Blank 

-- teken entiteiten
--drawAllEntities :: [Entity] -> Picture
--drawAllEntities ents = Pictures (map drawEntity ents)           -- ents is lijst entiteiten, functie drawEntity over lijst. Pictures, zie vorige functie

-- teken 1 entiteit
-- teken een player
drawPlayer :: Player -> Picture
drawPlayer p =
  translate x y $
  rotate rotDeg $
  color white $
  polygon [(-10, -10), (20, 0), (-10, 10)]
  where
    Movement (x, y) _ rotDeg = playerMovement p

drawEffects :: [Animation] -> Picture
drawEffects _ = Blank

drawEnemy :: Enemy -> Picture
drawEnemy e =
  translate x y $
  rotate rotDeg $
  color (colorFor (enemyType e)) $
  shapeFor (enemyType e) 
  where
    Movement (x, y) _ rotDeg = enemyMovement e

drawEnemies :: [Enemy] -> Picture
drawEnemies l = Pictures (map drawEnemy l)

drawBullet :: Bullet -> Picture
drawBullet b =
  translate x y $
  color yellow $
  circleSolid 2
  where
    Movement (x, y) _ _ = bulletMovement b

drawBullets :: [Bullet] -> Picture
drawBullets l = Pictures (map drawBullet l)

-- kleur van entiteit
colorFor :: EnemyType -> Color
colorFor ent = case ent of
  MeteorEnormous  -> greyN 0.7
  AsteroidLarge   -> greyN 0.6
  AsteroidMedium  -> greyN 0.5
  AsteroidSmall   -> greyN 0.4
  AsteroidMoving  -> greyN 0.3
  SaucerEnormous  -> red
  SaucerSmall     -> orange

-- entiteit vorm
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
  circle 15
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

-- pauze scherm
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
