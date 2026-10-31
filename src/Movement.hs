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

moveEnemy :: Float -> Point -> Enemy -> Enemy
moveEnemy dt point e =
  case enemyType e of                                                           --Movement gedrag van MovingAsteroid
    AsteroidMoving ->
      chasePlayer dt point e
    SaucerSmall ->
        e {enemyMovement = moveMovement dt ((enemyMovement e) {velocity = scaleVec 60 vel})}
    SaucerEnormous ->
        e { enemyMovement = moveMovement dt ((enemyMovement e) {velocity = scaleVec 30 vel})}
    _ ->                                                                        --Movement gedrag rest
      e { enemyMovement = 
            moveMovement dt (enemyMovement e) }
    where
      vel = scaleVec (1.0 / len (velocity $ enemyMovement e)) (velocity $ enemyMovement e)  

chasePlayer :: Float -> Point -> Enemy -> Enemy                                 --Laat MovingAsteroid de speler volgen
chasePlayer dt playerPos e =
  let
    m = enemyMovement e
    (ex, ey) = position m
    (px, py) = playerPos

    dx = px - ex
    dy = py - ey
    distance = len (dx, dy)                                                    --Afstand van asteroid en speler

    direction
      | distance == 0 = (0, 0)
      | otherwise = (dx / distance, dy / distance)                             --Normalized afstand

    acceleration = 20
    velocity' = velocity m `add` ((acceleration * dt) `scaleVec` direction)

    maxSpeed = 60
    speed = len velocity'
    velocity'' =
      if speed > maxSpeed
      then (maxSpeed / speed) `scaleVec` velocity'                              --limiteert de snelheid
      else velocity'
      
    m' = m { velocity = velocity'' }
  in
    e { enemyMovement = moveMovement dt m' }

moveBullet :: Float -> Bullet -> Bullet
moveBullet dt b =
  b { bulletMovement =
      (bulletMovement b) 
        {position = (x + vx * dt, y + vy * dt)}}
      where
        (x,y) = position $ bulletMovement b
        (vx,vy) = velocity $ bulletMovement b

   -- klaar
turnPlayer :: Float -> Player -> Player
turnPlayer deg p =
  p { playerMovement =
       m { rotation = rotation m + deg }
    }
    where
      m = playerMovement p

-- klaar
acceleratePlayer :: Float -> Player -> Player
acceleratePlayer amt p =
  let m = playerMovement p
      (vx, vy) = velocity m
      dir = -(rotation m * pi / 180)                                                --graden -> radialen
      newVel =
        (vx + cos dir * amt, vy + sin dir * amt)                                  -- nieuwe snelheid
  in  p { playerMovement =
            m { velocity = newVel }
        }

limitPlayerSpeed :: Player -> Player
limitPlayerSpeed p =
      let m = playerMovement p
          (vx,vy) = velocity m
          speed = len (vx,vy)
          maxSpeed = 60
          factor = maxSpeed / speed
      in if speed > maxSpeed                                                      --if speed exceeds max  
          then p {playerMovement =
                    m {velocity = factor `scaleVec` (vx, vy)}                   --scale velocity down
                 }
          else p

updatePlayerMovement :: Player -> Player
updatePlayerMovement p =
  let inp = playerInput p
      p1  = if turnLeft inp  then turnPlayer (-5) p     else p
      p2  = if turnRight inp then turnPlayer 5 p1       else p1
      p3  = if thrust inp    then acceleratePlayer 1 p2 else p2
  in p3

wrap :: Float -> Float -> Float -> Float                                                        
wrap mini maxi x | x < mini = maxi
                 | x > maxi = mini
                 | otherwise = x

wrapPos :: Point -> Point                                                                      --zorgt ervoor dat player of enemy aan de andere kant van het scherm terug komen.
wrapPos (x,y) = (wrap (-450) 450 x, wrap (-350) 350 y)