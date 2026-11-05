module Spawning where
import Model
import Control.Monad
import System.Random (randomRIO)
import Graphics.Gloss.Data.Point
import Shooting

--Check of een nieuwe wave nodig is
checkWave :: GameState -> IO GameState
checkWave gs
  | null (enemies gs) =
      spawnNewWave (wave gs + 1) gs
  | otherwise = pure gs

--Spawnt nieuwe enemies (meer elke wave)
spawnNewWave :: Int -> GameState -> IO GameState
spawnNewWave n gs = do 
  newEnemies <- replicateM (3+n) (spawnEnemy playerPos)
  pure $ gs { enemies = enemies gs ++ newEnemies, wave = n }
  where
    playerPos = map (position . playerMovement) (players gs)

-- genereert enemy positions die niet te dichtbij de player zijn
validSpawn :: [Point] -> IO Point
validSpawn playerPos = do
    x <- randomRIO (-400,400)
    y <- randomRIO (-300,300)
    if all (\p -> not (collides p 50 (x,y) 50)) playerPos
      then pure (x,y)
      else validSpawn playerPos

--Kiest welke enemies worden gespawnd
pickEnemy :: [(Int, EnemyType)] -> IO EnemyType
pickEnemy enWeights= do
  nr <- randomRIO (1, weightSum)
  if (nr - head weights) <= 0
    then pure (snd $ head enWeights)
    else pickEnemy $ tail enWeights
  where
    weightSum = sum weights
    weights = map fst enWeights

--Spawn een enemy 
spawnEnemy :: [Point] -> IO Enemy
spawnEnemy playerPos = do
  (x,y) <- validSpawn playerPos
  vx    <- randomRIO (-30,30)
  vy    <- randomRIO (-30,30)
  typ   <- pickEnemy enemyWeights
  pure $ Enemy
    typ                                           
    (Movement (x,y) (vx,vy) 0)
    (hpEnemy typ)
    (hitboxEnemy typ)

