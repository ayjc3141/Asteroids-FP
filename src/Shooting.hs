module Shooting where
import Data.List (foldl')
import Model
import Graphics.Gloss.Data.Point
import Animations

reduceHP :: Enemy -> Enemy
reduceHP e = e {enemyHitpoints = (enemyHitpoints e) - 1}

handleIframes :: Float -> Player -> Player 
handleIframes dt p = p {playerIframes = iframes}
  where 
    iframes = max 0 (playerIframes p - dt)

tryShoot :: Float -> Player -> (Player, [Bullet])
tryShoot dt p =                                                                                     -- check cooldown en vuur indien mogelijk
  let cd' = max 0 (playerCooldown p - dt)                                                           -- aftellen cooldown
  in if shoot (playerInput p) && cd' <= 0
        then (p {playerCooldown = 1.5}, [spawnBullet p])                                                -- cooldown klaar -> spawn bullet
        else (p {playerCooldown = cd'}, [])                                                             -- anders alleen aftellen


tryShootAll :: Float -> GameState -> GameState                                                      --probeert beide spelers tegelijk schieten
tryShootAll dt gs =
  let results = map (tryShoot dt) (players gs)
  in gs
       { players = map fst results
       , bullets = concatMap snd results ++ bullets gs
       }

setShoot :: Int -> Bool -> GameState -> GameState
setShoot nr val gs = gs {players = map setPlayer ps}
  where
    ps = players gs
    setPlayer p  
      | playerNr p == nr = p {playerInput = (playerInput p) {shoot = val}}                          --handelt shoot input
      | otherwise = p

spawnBullet :: Player -> Bullet                                                                     --spawnt bullet based on welke player het schiet
spawnBullet p =
  let m = playerMovement p
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
  in Bullet bulletMovement' Friend 2


spawnSaucerBullet :: Enemy -> Float -> Player -> GameState -> GameState
spawnSaucerBullet saucer angle player gs =
  let
    (sx, sy) = position (enemyMovement saucer)
    (px, py) = position (playerMovement player)
    dx       = px - sx
    dy       = py - sy
    distance = len (dx,dy)
    
    (dirX, dirY)                                                                                       --Richting waar de saucer schiet
      | enemyType saucer == SaucerSmall = rotateVec angle (dx / distance, dy / distance)               --offset angle van kleine saucers
      | otherwise = (dx / distance, dy / distance) 
    
    bulletMovement' =
      Movement
        (sx, sy)
        (dirX * 90, dirY * 90)
        0
    
    bullet = Bullet bulletMovement' Foe 2
  
  in gs { bullets = bullet : bullets gs }

tryShootSaucer :: Float -> Float -> GameState -> GameState
tryShootSaucer dt angle gs =                                                                       
  let cd'     = max 0 (saucercd gs - dt)
      saucers = filter isSaucer $ enemies gs
      p       = players gs
      
  in if any isSaucer (enemies gs) && cd' <= 0
        --saucer schiet op de dichtsbijzijnde speler (als het kan schieten)
        then foldl' (\state saucer -> spawnSaucerBullet saucer angle (closestPlayer saucer p) state) (gs {saucercd = 2.5}) saucers
        else gs {saucercd = cd'} 

closestPlayer :: Enemy -> [Player] -> Player                                                        --check dichtsbijzijnde speler van een enemy
closestPlayer e [p1, p2] 
  | min distToP1 distToP2 == distToP1 = p1
  | otherwise = p2
  where
      (x1,y1) = position $ playerMovement p1
      (x2,y2) = position $ playerMovement p2
      (x,y)         = position $ enemyMovement e
      distToP1      = len(x-x1, y-y1)
      distToP2      = len(x-x2, y-y2)

isSaucer :: Enemy -> Bool
isSaucer en =
  et `elem`                                                                                     
    [SaucerEnormous, SaucerSmall]
    where
      et = enemyType en

collides :: Point -> Float -> Point -> Float -> Bool
collides (x1, y1) r1 (x2, y2) r2 = len (x1 - x2, y1 - y2) < r1 + r2

hitsPlayer :: Player -> Enemy -> Bool
hitsPlayer p e = (collides (px, py) (playerHitbox p) (ex, ey) (enemyHitbox e)) && (playerIframes p == 0)
            where
                m = playerMovement p
                n = enemyMovement e
                (px, py) = position m
                (ex, ey) = position n

hitsAnyPlayer :: [Player] -> Enemy -> Bool            --Check of een van de twee spelers is gehit
hitsAnyPlayer ps e = any (`hitsPlayer` e) ps

handlePlayerCollisions :: GameState -> GameState
handlePlayerCollisions gs | null hit = gs
                          | otherwise = 
                              gs 
                                { effects = effects gs ++ map createPlayerHitEffect ps
                                , lives = lives gs -1
                                , players = map (checkIframes) ps}
                        where
                            ps  = players gs
                            hit = filter (hitsAnyPlayer ps) (enemies gs)
                            checkIframes p
                                = if any (hitsPlayer p) (enemies gs)
                                  then p {playerIframes = 3.0}
                                  else p

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
  in collides (bx,by) (bulletHitbox b) (px, py) (playerHitbox p) && (playerIframes p == 0)


handleBulletCollisions :: GameState -> GameState
handleBulletCollisions gs | null hit = gs
                          | otherwise = gs { enemies = newEnemies
                                           , bullets = newBullets
                                           , effects = effects gs ++ neweffect
                                           , score = score gs + 100}
                        where
                            bs            = bullets gs
                            es            = enemies gs
                            isHit e       = any (\b -> bulletType b == Friend && hitsBullet b e) bs --check of een bullet een enemy raakt
                            hit           = filter isHit es
                            reduced       = [if isHit e then reduceHP e else e | e <- es]           --reduce enemy hp als gethit
                            dead          = [e | e <- reduced, enemyHitpoints e <= 0]
                            neweffect     = [enemyEffect e | e <- dead]                             --plaats effect bij dode enemies
                            newEnemies    = [e | e <- reduced, enemyHitpoints e > 0]                --verwijder dode enemies
                            bulletsHit b  = any (\e -> bulletType b == Friend && hitsBullet b e) es 
                            newBullets    = filter (not . bulletsHit) bs                            --verwijder bullets die enemies raken

handleSaucBulletCollisions :: GameState -> GameState
handleSaucBulletCollisions gs = gs { players = newPlayers
                                   , lives = newLives
                                   , bullets = newBullets
                                   , effects = effects gs ++ newEffects}
                        where
                            bs            = bullets gs
                            ps            = players gs
                            isHit player  = any (\b -> bulletType b == Foe && bulletHitsPlayer b player) bs
                            checkIframe p | (isHit p) = p {playerIframes = 3.0} 
                                          | otherwise = p
                            newPlayers    = map checkIframe ps 
                            newLives      = if any isHit ps then (lives gs) - 1 else lives gs       --reduce player hp als een speler is geraakt
                            bulletsHit b  = bulletType b == Foe && any (bulletHitsPlayer b) ps      
                            newBullets    = filter (not . bulletsHit) bs                            --verwijder bullets die spelers raken
                            newEffects    = [createPlayerHitEffect p | p <- ps, isHit p]            --plaats effect op speler als geraakt


