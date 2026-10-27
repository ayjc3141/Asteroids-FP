module Movement where
import Model
import Graphics.Gloss.Data.Point

moveMovement :: Float -> Movement -> Movement
moveMovement dt m =
  let (x, y) = position m
      (vx, vy) = velocity m
  in m
       { position = wrapPos (x + vx * dt, y + vy * dt) }

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
      (bulletMovement b) 
        {position = (x + vx * dt, y + vy * dt)}}
      where
        (x,y) = position $ bulletMovement b
        (vx,vy) = velocity $ bulletMovement b

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

wrap :: Float -> Float -> Float -> Float                                                        
wrap mini maxi x | x < mini = maxi
                 | x > maxi = mini
                 | otherwise = x

wrapPos :: Point -> Point                                                                      --zorgt ervoor dat player of enemy aan de andere kant van het scherm terug komen.
wrapPos (x,y) = (wrap (-450) 450 x, wrap (-350) 350 y)