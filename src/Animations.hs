module Animations where

import Model
import Graphics.Gloss.Data.Point
import View(colorFor)
import Graphics.Gloss

--Grotere enemies krijgen grotere effects on death
effectSize :: EnemyType -> [(Point, Point)]
effectSize enTyp | (enTyp == SaucerEnormous) || (enTyp == MeteorEnormous) = pattern ++ pattern2
                 | otherwise = pattern
                 where
                  pattern2       = map (\(p,v) -> (p, 2 `scaleVec` v)) pattern

--Beweegt een particle (lijn) naar buiten
moveParticle :: Float -> (Point, Point) -> (Point, Point)
moveParticle dt (pos, vel) =
    (pos `add` (dt `scaleVec` vel), vel)

--Beweegt alle particles van een effect
moveParticles :: Float -> [(Point, Point)] -> [(Point, Point)]
moveParticles _ [] = []
moveParticles dt (x:xs) = (moveParticle dt x) : (moveParticles dt xs)     

--Update effect duration en plek
updateAnimation :: Float -> Animation -> Animation
updateAnimation dt anim =
    anim
      { duration = duration anim - dt
      , particles = moveParticles dt (particles anim)
      }

--Update alle effects en haal animaties eruit die voorbij zijn
updateAnimations :: Float -> [Animation] -> [Animation]
updateAnimations dt animations =
    filter ((> 0) . duration) (map (updateAnimation dt) animations)

--Beweegt effect naar positie
moveEffect :: Point -> [(Point, Point)] -> [(Point, Point)]
moveEffect _ [] = []
moveEffect entityPos ((pos,vel):xs) =
    ((pos `add` entityPos), vel) : (moveEffect entityPos xs)

--Maak enemy effect
enemyEffect :: Enemy -> Animation
enemyEffect e = 
    Animation 
        pos 
        0.5 
        EnemyDeath 
        (moveEffect pos $ effectSize (enemyType e)) 
        (colorFor $ enemyType e)
        where
            pos = position (enemyMovement e)

--Maak effect als player gehit wordt
createPlayerHitEffect :: Player -> Animation
createPlayerHitEffect p = 
    Animation
    pos
    0.1
    PlayerHit
    (moveEffect pos playerHitPattern)
    red
        where  
            pos = position (playerMovement p)

--Effect patronen die gebruikt worden.
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

