module Animations where

import Model
import Graphics.Gloss.Data.Point

moveEffect :: Enemy -> [(Point, Point)] -> [(Point, Point)]
moveEffect _ [] = []
moveEffect en ((pos,vel):xs) =
    ((pos `add` p), vel) : (moveEffect en xs)
    where
        p = position $ enemyMovement en

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
    
chooseEffect :: EnemyType -> AnimationType
chooseEffect enTyp | enTyp == SaucerSmall || enTyp == SaucerEnormous = Saucer
                   | otherwise = Asteroid