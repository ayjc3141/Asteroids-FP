module Model where
import Graphics.Gloss.Data.Point

-- data spel, huidige toestand (state)
-- alle types
-- ik mis volgensmij wel ergens iets maar weet niet wat, niet goed naar gekeken.



--functies voor points
add :: Point -> Point -> Point
add (x0,y0) (x1,y1) = (x0+x1,y0+y1)

scaleVec :: Float -> Point -> Point
scaleVec t (x0,y0) = (t*x0, t*y0)

len :: Point -> Float
len (x,y) = sqrt (x*x + y*y)

data Movement = Movement 
  { position :: Point
  , velocity :: Point
  , rotation :: Float 
  } deriving (Show, Eq)

-- alle enemies in spel
data Enemy = Enemy 
  { enemyType      :: EnemyType
  , enemyMovement  :: Movement 
  , enemyHitpoints :: Int
  , enemyAlive     :: Bool
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

enemyWeights :: [(Int, EnemyType)]      --elke getal wijst naar de kans dat een enemy van dat type spawnt
enemyWeights =  [ (20, AsteroidSmall)
                , (20, AsteroidMedium)
                , (35, AsteroidLarge)
                , (5, MeteorEnormous)
                , (5, AsteroidMoving)
                , (10, SaucerSmall)
                , (5, SaucerEnormous)]

hpEnemy :: EnemyType -> Int
hpEnemy MeteorEnormous = 2
hpEnemy SaucerEnormous = 3
hpEnemy _ = 1

hitboxEnemy :: EnemyType -> Float
hitboxEnemy AsteroidSmall   = 12
hitboxEnemy AsteroidMedium  = 20
hitboxEnemy AsteroidLarge   = 30
hitboxEnemy AsteroidMoving  = 15
hitboxEnemy MeteorEnormous  = 42
hitboxEnemy SaucerSmall     = 18
hitboxEnemy SaucerEnormous  = 25

-- beschrijving player
data Player = Player 
  { playerNr            :: Int
  , playerMovement      :: Movement
  , playerAlive         :: Bool
  , playerHitbox        :: Float 
  , playerInput         :: Input
  } deriving (Show, Eq)

-- bullets
data BulletType = Friend | Foe deriving (Show, Eq)
data Bullet = Bullet 
  { bulletMovement :: Movement
  , bulletType     :: BulletType 
  , bulletHitbox   :: Float
  } deriving (Show,Eq)
-- invoer speler
data Input = Input
  { turnLeft  :: Bool
  , turnRight :: Bool
  , thrust    :: Bool
  , shoot     :: Bool
  } deriving (Show, Eq)

-- speltoestand
data GameState = GameState 
  { players :: Player
  , enemies :: [Enemy]
  , bullets :: [Bullet]
  , effects :: [Animation]
  , score   :: Int
  , lives   :: Int
  , wave    :: Int
  , paused  :: Bool
  , cooldown:: Float
  , saucercd:: Float
  } deriving (Show)

--animaties
data AnimationType = Saucer | Asteroid deriving (Show, Eq)

data Animation = Animation 
  { animationPosition  :: Point
  , duration           :: Float
  , animationType      :: AnimationType
  , particles          :: [(Point,Point)]
  } deriving (Show, Eq)

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
-- score
data HighScore = HighScore
  { playerName  :: String
  , playerScore :: Int
  } deriving (Show, Eq, Ord)

-- beginstaat spel
initialGameState :: GameState
initialGameState =
  GameState
    { players = Player 1 (Movement (0,0) (0,0) 0) True 5 (Input False False False False)
    , enemies = []
    , bullets = []
    , effects = []
    , score   = 0
    , lives   = 3
    , wave    = 0
    , paused  = False
    , cooldown = 0                                      -- nieuw
    , saucercd = 0
    }
