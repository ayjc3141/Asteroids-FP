module Animations where

import Model
import Graphics.Gloss.Data.Point
import View(colorFor)
import Graphics.Gloss

moveEffect :: Point -> [(Point, Point)] -> [(Point, Point)]
moveEffect _ [] = []
moveEffect entityPos ((pos,vel):xs) =
    ((pos `add` entityPos), vel) : (moveEffect entityPos xs)

effectSize :: EnemyType -> [(Point, Point)]
effectSize enTyp | (enTyp == SaucerEnormous) || (enTyp == MeteorEnormous) = pattern ++ pattern2
                 | otherwise = pattern
                 where
                  ((pos,vel):xs) = pattern
                  pattern2       = map (\(p,v) -> (p, 2 `scaleVec` v)) ((pos,vel):xs) 

moveParticle :: Float -> (Point, Point) -> (Point, Point)
moveParticle dt (pos, vel) =
    (pos `add` (dt `scaleVec` vel), vel)

moveParticles :: Float -> [(Point, Point)] -> [(Point, Point)]
moveParticles _ [] = []
moveParticles dt (x:xs) = (moveParticle dt x) : (moveParticles dt xs)     

updateAnimation :: Float -> Animation -> Animation
updateAnimation dt anim =
    anim
      { duration = duration anim - dt
      , particles = moveParticles dt (particles anim)
      }

updateAnimations :: Float -> [Animation] -> [Animation]
updateAnimations dt animations =
    filter ((> 0) . duration) (map (updateAnimation dt) animations)         --Haalt alle effects die voorbij zijn 

enemyEffect :: Enemy -> Animation
enemyEffect e = 
    Animation 
        (position $ enemyMovement e) 
        0.5 
        EnemyDeath 
        (moveEffect pos $ effectSize (enemyType e)) 
        (colorFor $ enemyType e)
        where
            pos = position (enemyMovement e)

pattern :: [(Point, Point)]
pattern = 
    [ ((-5,  0) , (-50,  10))
    , (( 5,  0) , ( 50, -10))
    , (( 0,  5) , ( 10,  50))
    , (( 0, -5) , (-10, -50))
    , (( 3,  3) , ( 40,  40))
    , ((-3, -3) , (-40, -40))
    , ((-3,  3) , (-40,  40))
    , (( 3, -3) , ( 40, -40))]

playerHitPattern :: [(Point, Point)]
playerHitPattern =
    [ ((-3,  0), (-100,  20))
    , (( 3,  0), ( 100, -20))
    , (( 0,  3), (  20, 100))
    , (( 0, -3), ( -20,-100))
    , ((-2,  2), ( -80,  80))
    , (( 2, -2), (  80, -80))
    ]

createPlayerHitEffect :: Player -> Animation
createPlayerHitEffect p =
    let pos = position (playerMovement p)
    in Animation
        pos
        0.1
        PlayerHit
        (moveEffect pos playerHitPattern)
        red