module Model where
import Graphics.Gloss.Data.Point
import Graphics.Gloss

-- data spel, huidige toestand (state)
-- alle types
-- ik mis volgensmij wel ergens iets maar weet niet wat, niet goed naar gekeken.



--Functies voor points
add :: Point -> Point -> Point
add (x0,y0) (x1,y1) = (x0+x1,y0+y1)

scaleVec :: Float -> Point -> Point
scaleVec t (x0,y0) = (t*x0, t*y0)

len :: Point -> Float
len (x,y) = sqrt (x*x + y*y)

rotateVec :: Float -> Point -> Point
rotateVec degrees (x,y) =
    (x * cos a - y * sin a,
     x * sin a + y * cos a)
  where
    a = degrees * pi / 180

data Movement = Movement 
  { position :: Point
  , velocity :: Point
  , rotation :: Float 
  } deriving (Show, Eq)

-- Alle enemies in spel
data Enemy = Enemy 
  { enemyType      :: EnemyType
  , enemyMovement  :: Movement 
  , enemyHitpoints :: Int
  , enemyHitbox    :: Float
  } deriving (Show, Eq)

data EnemyType
  = MeteorEnormous
  | AsteroidLarge
  | AsteroidMedium
  | AsteroidSmall
  | AsteroidMoving
  | SaucerEnormous
  | SaucerSmall
  deriving (Show, Eq)


pointsFor :: EnemyType -> Int
pointsFor MeteorEnormous = 20
pointsFor AsteroidLarge  = 20
pointsFor AsteroidMedium = 50
pointsFor AsteroidSmall  = 100
pointsFor AsteroidMoving = 150
pointsFor SaucerEnormous = 200
pointsFor SaucerSmall    = 500

--De kans voor elke soort enemy om te spawnen
enemyWeights :: [(Int, EnemyType)]      
enemyWeights =  [ (20, AsteroidSmall)
                , (20, AsteroidMedium)
                , (35, AsteroidLarge)
                , (5, MeteorEnormous)
                , (10, AsteroidMoving)
                , (13, SaucerSmall)
                , (2, SaucerEnormous)]

--Hit points voor elke enemy type
hpEnemy :: EnemyType -> Int
hpEnemy MeteorEnormous = 2
hpEnemy SaucerEnormous = 3
hpEnemy _ = 1

--Hitboxes
hitboxEnemy :: EnemyType -> Float
hitboxEnemy AsteroidSmall   = 12
hitboxEnemy AsteroidMedium  = 20
hitboxEnemy AsteroidLarge   = 30
hitboxEnemy AsteroidMoving  = 15
hitboxEnemy MeteorEnormous  = 42
hitboxEnemy SaucerSmall     = 18
hitboxEnemy SaucerEnormous  = 25

-- Player
data Player = Player 
  { playerNr            :: Int
  , playerMovement      :: Movement
  , playerIframes       :: Float
  , playerHitbox        :: Float 
  , playerInput         :: Input
  , playerCooldown      :: Float  --Schiet cooldown
  } deriving (Show, Eq)

-- Bullets
data BulletType = Friend | Foe deriving (Show, Eq)
data Bullet = Bullet 
  { bulletMovement :: Movement
  , bulletType     :: BulletType 
  , bulletHitbox   :: Float
  } deriving (Show,Eq)

-- Invoer speler
data Input = Input
  { turnLeft  :: Bool
  , turnRight :: Bool
  , thrust    :: Bool
  , shoot     :: Bool
  } deriving (Show, Eq)

-- Speltoestand
data GameState = GameState 
  { players :: [Player]
  , enemies :: [Enemy]
  , bullets :: [Bullet]
  , effects :: [Animation]
  , score   :: Int
  , lives   :: Int
  , wave    :: Int
  , paused  :: Bool
  , saucercd:: Float  --Schiet cooldown van saucers
  , highScores :: [Int]
  } deriving Show

--Animaties
data AnimationType = EnemyDeath | PlayerHit deriving (Show, Eq)

data Animation = Animation 
  { animationPosition  :: Point
  , duration           :: Float
  , animationType      :: AnimationType
  , particles          :: [(Point,Point)]
  , effectColor        :: Color
  } deriving (Show, Eq)

-- Score
data HighScore = HighScore
  { playerName  :: String
  , playerScore :: Int
  } deriving (Show, Eq, Ord)

-- Beginstaat spel
initialGameState :: GameState
initialGameState =
  GameState
    { players = [ Player 1 (Movement (-50,0) (0,0) 0) 0 7 (Input False False False False) 0
                , Player 2 (Movement ( 50,0) (0,0) 180) 0 7 (Input False False False False) 0]
    , enemies = []
    , bullets = []
    , effects = []
    , score   = 0
    , lives   = 3
    , wave    = 0
    , paused  = False                                     
    , saucercd = 0
    , highScores = []
    }
