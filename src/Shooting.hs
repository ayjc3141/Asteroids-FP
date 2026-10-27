module Shooting where
import Data.List (foldl')
import Model
import Graphics.Gloss.Data.Point

reduceHP :: Enemy -> Enemy
reduceHP e = e {enemyHitpoints = (enemyHitpoints e) - 1}

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
        (vx + cos dir * 90, vy + sin dir * 90)

      bulletMovement' =
        Movement bulletPosition bulletVelocity (rotation m)

      bullet =
        Bullet bulletMovement' Friend 2

  in gs { bullets = bullet : bullets gs }



spawnSaucerBullet :: Enemy -> Player -> GameState -> GameState
spawnSaucerBullet saucer player gs =
  let
    (sx, sy) = position (enemyMovement saucer)
    (px, py) = position (playerMovement player)
    dx       = px - sx
    dy       = py - sy
    distance = len (dx,dy)
    (dirX, dirY) = (dx / distance, dy / distance) --Richting waar de saucer schiet

    bulletMovement' =
      Movement
        (sx, sy)
        (dirX * 90, dirY * 90)
        0
    bullet = Bullet bulletMovement' Foe 2
  in gs { bullets = bullet : bullets gs }

tryShootSaucer :: Float -> GameState -> GameState
tryShootSaucer dt gs =                                                                       
  let cd'     = max 0 (saucercd gs - dt)
      saucers = filter isSaucer $ enemies gs
      p       = players gs

  in if any isSaucer (enemies gs) && cd' <= 0
        then foldl' (\state saucer -> spawnSaucerBullet saucer p state) (gs {saucercd = 2.5}) saucers
        else gs {saucercd = cd' } 


isSaucer :: Enemy -> Bool
isSaucer en =
  et `elem`                                                                                     -- check of entitytype een asteroid is
    [SaucerEnormous, SaucerSmall]
    where
      et = enemyType en

collides :: Point -> Float -> Point -> Float -> Bool
collides (x1, y1) r1 (x2, y2) r2 = len (x1 - x2, y1 - y2) < r1 + r2

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
                            p    = players gs
                            hit  = filter (hitsPlayer p) (enemies gs)
                            rest = filter (not . hitsPlayer p) (enemies gs)

hitsBullet :: Bullet -> Enemy -> Bool
hitsBullet b e = collides (bx, by) (bulletHitbox b) (ex, ey) (enemyHitbox e)
    where
        m        = bulletMovement b
        n        = enemyMovement e
        (bx, by) = position m
        (ex, ey) = position n

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer b p = 
  let 
    bm       = bulletMovement b
    pm       = playerMovement p
    (bx, by) = position bm
    (px, py) = position pm
  in collides (bx,by) (bulletHitbox b) (px, py) (playerHitbox p)


handleBulletCollisions :: GameState -> GameState
handleBulletCollisions gs | null hit = gs
                          | otherwise = gs {enemies = rest, bullets = restb, score = score gs + 100}
                        where
                            bs            = bullets gs
                            es            = enemies gs
                            isHit e       = any (\b -> bulletType b == Friend && hitsBullet b e) bs
                            hit           = map reduceHP (filter isHit es)
                            reduced       = [if isHit e then reduceHP e else e | e <- es]
                            rest          = [e | e <- reduced, enemyHitpoints e > 0]
                            bulletsHit b  = any (\e -> bulletType b == Friend && hitsBullet b e) es
                            restb         = filter (not . bulletsHit) bs

handleSaucBulletCollisions :: GameState -> GameState
handleSaucBulletCollisions gs = gs {lives = rest, bullets = restb}
                        where
                            bs            = bullets gs
                            p             = players gs
                            isHit p       = any (\b -> bulletType b == Foe && bulletHitsPlayer b p) bs
                            rest          = if isHit p then (lives gs) - 1 else lives gs
                            bulletsHit b  = (\p -> bulletType b == Foe && bulletHitsPlayer b p) p
                            restb         = filter (not . bulletsHit) bs
