module Data where

import Graphics.Gloss

type CellCoord = (Int,Int)
type Texture = Picture

data SightDirection = SDLeft | SDRight | SDUp | SDDown
  deriving (Eq,Show)
data BombPhase = BStart | BSteady | BBoom
  deriving (Eq,Show)

data GameObject = Hero { hsDir :: SightDirection }
                | Enemy { esDir :: SightDirection
                        , eDelta :: Float }
                | Wall
                | DesWall 
                | Bomb { bPhaseLiveTime :: Float
                       , bPhase :: BombPhase }
  deriving (Eq,Show)
type Cell = (GameObject,CellCoord)

data World = World
  { field :: [Cell]
  , wTextures :: [[Texture]]
  } deriving (Eq,Show)


-- | just short form of fromIntegral function
fI :: (Num b, Integral a) => a -> b
fI = fromIntegral

-- | of cource this function has to apply only on (Just _) vals
getMaybeVal :: Maybe a -> a
getMaybeVal (Just v) = v
getMaybeVal Nothing = undefined

fps :: Int
fps = 10

widthOfWallTexture :: Int
widthOfWallTexture = 45

heightOfWallTexture :: Int
heightOfWallTexture = 45


windowWidth :: Int
windowWidth = worldWidth * widthOfWallTexture 

windowHeight :: Int
windowHeight = worldHeight * heightOfWallTexture 


worldWidth :: Int
worldWidth = 15

worldHeight :: Int
worldHeight = 10


wallWidth :: Int
wallWidth = widthOfWallTexture 

wallHeight :: Int 
wallHeight = heightOfWallTexture



blankWorld :: World
blankWorld = World [] [[Blank]]

basicEnemyVelocity :: (Fractional a) => a
basicEnemyVelocity = 3

bgColor :: Color
bgColor = mixColors 0.38 0.62 white yellow

labyrinth :: [CellCoord]
labyrinth = [(x,y) |
    x <- [2,4..worldWidth - 3], y <- [2,4..worldHeight - 3]]

explRadius :: Int
explRadius = 2
