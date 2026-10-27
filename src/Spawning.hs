module Spawning where
import Model
import Control.Monad
import System.Random (randomRIO)
import Graphics.Gloss.Data.Point
import Shooting


checkWave :: GameState -> IO GameState
checkWave gs
  | null (enemies gs) =                                                                         --geen enemy?
      spawnNewWave (wave gs + 1) gs                                                             --dan nieuwe wave
  | otherwise = pure gs                                                                         --anders niks doen

spawnNewWave :: Int -> GameState -> IO GameState
spawnNewWave n gs = do 
  newEnemies <- replicateM (3+n) (spawnEnemy playerPos)
  pure $ gs { enemies = enemies gs ++ newEnemies, wave = n }                                    -- Voeg toe aan game
  where
    playerPos = position $ playerMovement $ players $ gs

validSpawn :: Point -> IO Point                                                                 -- genereert enemy positions die niet te dichtbij de player zijn
validSpawn playerPos = do
    x <- randomRIO (-400,400)
    y <- randomRIO (-300,300)
    if not (collides playerPos 50 (x,y) 50)
      then pure (x,y)
      else validSpawn playerPos

pickEnemy :: [(Int, EnemyType)] -> IO EnemyType
pickEnemy enWeights= do
  nr <- randomRIO (1, weightSum)
  if (nr - head weights) <= 0
    then pure (snd $ head enWeights)
    else pickEnemy $ tail enWeights
  where
    weightSum = sum weights
    weights = map fst enWeights


spawnEnemy :: Point -> IO Enemy
spawnEnemy playerPos = do
  (x,y) <- validSpawn playerPos
  vx    <- randomRIO (-30,30)
  vy    <- randomRIO (-30,30)
  typ   <- pickEnemy enemyWeights
  pure $ Enemy
    typ                                           
    (Movement (x,y) (vx,vy) 0)
    (hpEnemy typ)
    True
    (hitboxEnemy typ)