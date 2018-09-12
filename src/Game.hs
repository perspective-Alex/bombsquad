module Game where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.List (intersect)

import Data

objPic :: CellCoord -> Picture -> Picture
objPic (x,y) img = translate newX newY img
  where
    (newX,newY) = translateCoord (x,y)

worldPic :: World -> Picture
worldPic (World f
            [[ctx],
             [wtx,dwtx],
             [hDowntx,hUptx,hLefttx,hRighttx],
             [eDowntx,eUptx,eLefttx,eRighttx],
             [b1tx,b2tx,b3tx]]
         ) =
  Pictures [field,grid] 
    where
      field = Pictures $ map (gameObjPic) f
      gameObjPic :: Cell -> Picture
      gameObjPic (go,xy) =
        case go of
          Wall           -> objPic xy wtx
          DesWall        -> objPic xy dwtx
          Bomb t BStart  -> objPic xy b1tx
          Bomb t BSteady -> objPic xy b2tx
          Bomb t BBoom   -> objPic xy b3tx
          Hero SDDown    -> objPic xy hDowntx
          Hero SDUp      -> objPic xy hUptx
          Hero SDLeft    -> objPic xy hLefttx
          Hero SDRight   -> objPic xy hRighttx
          Enemy SDDown _   -> objPic xy eDowntx
          Enemy SDUp _     -> objPic xy eUptx
          Enemy SDLeft _   -> objPic xy eLefttx
          Enemy SDRight _   -> objPic xy eRighttx
      grid = Pictures $ map (\c -> objPic c ctx) allCells
      allCells = [(x,y) |
                  x <- [0..worldWidth - 1], y <- [0..worldHeight - 1]]


handleEvents :: Event -> World -> World
handleEvents event w =
  if (oldX, oldY) == (-1,-1)
  then w
  else
    case event of
      EventKey (Char 'd') Down _ _ -> 
        let { newCoord = (oldX + 1, oldY) } in
        if freeCell newCoord $ field w
        then w {field = updateCell newCoord (Hero SDRight) $
                        updateHeroCoord newCoord $ field w
               }
        else w {field = updateCell (oldX, oldY) (Hero SDRight) $ field w}
      EventKey (Char 'a') Down _ _ -> 
        let { newCoord = (oldX - 1, oldY) } in
        if freeCell newCoord $ field w
        then w {field = updateCell newCoord (Hero SDLeft) $
                        updateHeroCoord newCoord $ field w
               }
        else w {field = updateCell (oldX, oldY) (Hero SDLeft) $ field w}
      EventKey (Char 'w') Down _ _ -> 
        let { newCoord = (oldX, oldY + 1) } in
        if freeCell newCoord $ field w
        then w {field = updateCell newCoord (Hero SDUp) $
                        updateHeroCoord newCoord $ field w
               }
        else w {field = updateCell (oldX, oldY) (Hero SDUp) $ field w}
      EventKey (Char 's') Down _ _ -> 
        let { newCoord = (oldX, oldY - 1) } in
        if freeCell newCoord $ field w
        then w {field = updateCell newCoord (Hero SDDown) $
                        updateHeroCoord newCoord $ field w
               }
        else w {field = updateCell (oldX, oldY) (Hero SDDown) $ field w}
 
      EventKey (SpecialKey KeyLeft) Down _ _ ->
        w {field = updateCell (oldX, oldY) (Hero SDLeft) $ field w}
      EventKey (SpecialKey KeyRight) Down _ _ ->
        w {field = updateCell (oldX, oldY) (Hero SDRight) $ field w}
      EventKey (SpecialKey KeyUp) Down _ _ ->
        w {field = updateCell (oldX, oldY) (Hero SDUp) $ field w}
      EventKey (SpecialKey KeyDown) Down _ _ ->
        w {field = updateCell (oldX, oldY) (Hero SDDown) $ field w}
 
 
      EventKey (SpecialKey KeySpace) Down _ _ ->
        let
          { maybeSD = getHeroSightDirection $ field w
          ; newCoord =
              case maybeSD of
                Just sd -> convertHeroSDtoAdjCell sd (oldX, oldY)
                Nothing -> (0,0)
          } in
        if freeCell newCoord $ field w
        then w {field = [(Bomb 0.0 BStart, newCoord)] ++ field w}
        else w
 
      _ -> w
    where
      (oldX,oldY) = 
        case lookupHero (field w) of
          Just coords -> coords
          _ -> (-1,-1)

updateScreen :: Float -> World -> World
updateScreen time w =
  w { field = updateEnemies time bombs
              $ execBooms $ updateBombs time (field w) }
  where
    bombs = findBombs $ field w


--updateScreen :: Float -> World -> World
--updateScreen s w = 
--  if distance >= 1
--  then World [(x,y)] (wTextures w) 0.0
--  else w {wDelta = newDelta}
--  where
--    ((oldX,oldY) : _) = walls w
--    (x,y) = (round (fI oldX + distance) `mod` 10, oldY)
--    distance = basicVelocity * (wDelta w + s)
--     newDelta = wDelta w + s

-------------------------------------World---------------------------------

translateCoord :: CellCoord -> (Float,Float)
translateCoord (x,y) = (fI (x * wallWidth) - 
                       fI (windowWidth - wallWidth) / 2.0,
                       fI (y * wallHeight) - 
                       fI (windowHeight - wallHeight) / 2.0)


initWorld :: IO World
initWorld = do
  let
    { cellImages  = ["./images/cell.bmp"]
    ; wallImages  = ["./images/wall.bmp", "./images/wall02.bmp"]
    ; heroImages  = ["./images/guy_down.bmp", 
                    "./images/guy_up.bmp", 
                    "./images/guy_left.bmp", 
                    "./images/guy_right.bmp"]
    ; enemyImages = ["./images/2guy_down.bmp",
                    "./images/2guy_up.bmp", 
                    "./images/2guy_left.bmp", 
                    "./images/2guy_right.bmp"]
    ; bombImages  = ["./images/bomb1.bmp","./images/bomb2.bmp",
                     "./images/bomb3.bmp"]
    }
  cellPictures <- mapM loadBMP cellImages
  wallPictures <- mapM loadBMP wallImages
  heroPictures <- mapM loadBMP heroImages
  enemyPictures <- mapM loadBMP enemyImages
  bombPictures <- mapM loadBMP bombImages
  return $ World initField
   [cellPictures,wallPictures,heroPictures,enemyPictures,bombPictures]

initField :: [Cell]
initField = allWalls ++ [hero,enemy1,enemy2]
  where
    wCoords = [(x,y) | x <- [0..worldWidth - 1], y <- [0..worldHeight - 1],
                      x == 0 || y == 0 ||
                      x == worldWidth - 1 || y == worldHeight - 1]
    walls = map (\c -> (Wall,c)) wCoords
    dWalls = map (\c -> (DesWall,c)) labyrinth
    allWalls = walls ++ dWalls
    hero = (Hero SDDown, (1,1))
    enemy1 = (Enemy SDDown 0.0, (worldWidth - 2,worldHeight - 2))
    enemy2 = (Enemy SDDown 0.0, (worldWidth - 2,1))

freeCell :: CellCoord -> [Cell] -> Bool
freeCell (x,y) field = not $ (x,y) `elem` coords 
  where
    coords = map snd field

updateCell :: CellCoord -> GameObject -> [Cell] -> [Cell]
updateCell cc newGO field = map (modifyCell) field
  where
    modifyCell c =
      if snd c == cc
      then (newGO, cc)
      else c

moveEnemy :: CellCoord -> CellCoord -> [Cell] -> [Cell]
moveEnemy _ _ [] = []
moveEnemy newCC oldCC (x@(go,cc):xs) =
  if cc == oldCC
  then if (dX,dY) == (0,0)
       then (go {eDelta = 0.0}, oldCC) : xs
       else (Enemy newSD 0.0, newCC) : xs
  else x : moveEnemy newCC oldCC xs
    where
      (dX,dY) = (fst newCC - fst oldCC, snd newCC - snd oldCC)
      newSD = if dX == 0
              then if dY <= 0
                   then SDDown
                   else SDUp
              else if dX < 0
                   then SDLeft
                   else SDRight

-- | find cells which take place near the given one (distance <= 2)
aroundCell :: CellCoord -> [Cell] -> [CellCoord]
aroundCell (x1,y1) field = map (snd) $
  filter (\(go,(x2,y2)) -> (abs (x2 - x1) + abs (y2 - y1)) <= 2) field

-- | find cells which take place near the given one (distance <= 3)
aroundCelld3 :: CellCoord -> [Cell] -> [CellCoord]
aroundCelld3 (x1,y1) field = map (snd) $
  filter (\(go,(x2,y2)) -> (abs (x2 - x1) + abs (y2 - y1)) <= 3) field

--------------------------------HERO---------------------------------------
lookupHero :: [Cell] -> Maybe CellCoord 
lookupHero [] = Nothing
lookupHero ((go,cc):xs) =
  case go of
    Hero smth -> Just cc
    _ -> lookupHero xs

updateHeroCoord :: CellCoord -> [Cell] -> [Cell]
updateHeroCoord newCoords field = map (changeHeroCoord newCoords) field 
  where
    changeHeroCoord newCoords c =
      case fst c of
        Hero sd ->  (Hero sd, newCoords)
        _ -> c

getHeroSightDirection :: [Cell] -> Maybe SightDirection
getHeroSightDirection [] = Nothing
getHeroSightDirection ((go,_):xs) =
  case go of
    Hero sd -> Just sd
    _ -> getHeroSightDirection xs

convertHeroSDtoAdjCell :: SightDirection -> CellCoord -> CellCoord
convertHeroSDtoAdjCell sd (x,y) =
  case sd of
   SDUp    -> (x,y+1) 
   SDDown  -> (x,y-1) 
   SDRight -> (x+1,y) 
   SDLeft  -> (x-1,y) 

----------------------------------Bombs------------------------------------

updateBombs :: Float -- ^ passed time since last updateScreen execution
            -> [Cell]
            -> [Cell]
updateBombs t field = map (updBombs) field
  where
    updBombs (go,cc) =
      case go of
        Bomb curt BStart ->
          if curt + t >= 1.0
          then (Bomb 0.0 BSteady, cc)
          else (Bomb (curt + t) BStart, cc)
        Bomb curt BSteady ->
          if curt + t >= 1.0
          then (Bomb 0.0 BBoom, cc)
          else (Bomb (curt + t) BSteady, cc)
        Bomb curt BBoom ->
          (Bomb (curt + t) BBoom, cc)

        _ -> (go,cc)
    deleteBoomed :: [Cell] -> [Cell]
    deleteBoomed [] = []
    deleteBoomed (x@(go,cc):xs) =
      case go of
        Bomb curt BBoom ->
          if curt >= 0.5
          then deleteBoomed xs
          else x:deleteBoomed xs
        _ -> x:deleteBoomed xs

-- | destroy all objects around the explosions
-- (objects which actually suffer) = everything beside walls
execBooms :: [Cell] -> [Cell]
execBooms [] = []
execBooms field = executeBooms (findExplosions field) field
  where
    executeBooms :: [CellCoord] -> [Cell] -> [Cell]
    executeBooms [] f = f
    executeBooms (c:cs) field = executeBooms cs (execOneBoom c field)

    execOneBoom (x1,y1) field =
      filter (\(go,(x2,y2)) ->
        (abs (x2 - x1) + abs (y2 - y1)) > explRadius || go == Wall) field


findExplosions :: [Cell] -> [CellCoord]
findExplosions field =
  map (snd) $ filter (\(go,_) -> case go of
                                   Bomb curt BBoom -> curt >= 0.5
                                   _ -> False ) field
findBombs :: [Cell] -> [CellCoord]
findBombs field =
  map (snd) $ filter (\(go,_) -> case go of
                                   Bomb curt _ -> True 
                                   _ -> False ) field

-------------------------------ENEMY---------------------------------------

getEnemiesCoordsAndDeltas :: [Cell] -> [(Float, CellCoord)]
getEnemiesCoordsAndDeltas field = map (\(g,cc) -> (eDelta g, cc)) $
  filter (\(go,_) -> case go of
                      Enemy _ _ -> True
                      _ -> False
         ) field

updateEnemies :: Float
              -> [CellCoord] -- ^ coordinates of bombs
              -> [Cell]
              -> [Cell]
updateEnemies delta bc field = 
  case (lookupHero field) of
    Nothing -> field
    Just hcc -> updEnemies (hcc) bc
                  (getEnemiesCoordsAndDeltas field) (incEnemiesDelta field)
  where
    incEnemiesDelta =
      map (\(go,cc) -> case go of
                        Enemy sd d -> (Enemy sd (d+delta), cc)
                        _ -> (go,cc)) 
    updEnemies :: CellCoord
               -> [CellCoord]
               -> [(Float, CellCoord)]
               -> [Cell]
               -> [Cell]
    updEnemies _ _ [] field = field
    updEnemies hcc bc (c:cs) field = updEnemies hcc bc cs
                                    (updOneEnemy hcc bc c field)

    updOneEnemy (hX,hY) bc (t,ecc@(eX,eY)) field =
      if distance >= 1.0 
      then if nearBombs == []
           then moveEnemy newCC ecc field
           else moveEnemy backWayCC ecc field
      else field
        where
          distance = basicEnemyVelocity * t
          step = round distance
          (d1x,d1y) = (hX - eX, hY - eY)
          newCC = if abs d1x > abs d1y
                  then if d1x < 0
                       then if freeCell (eX - step, eY) field
                            then (eX - step, eY)
                            else (eX,eY)
                       else if freeCell (eX + step, eY) field
                            then (eX + step, eY)
                            else (eX,eY)
                  else if d1y <= 0
                       then if freeCell (eX, eY - step) field
                            then (eX, eY - step)
                            else (eX,eY)
                       else if freeCell (eX, eY + step) field
                            then (eX, eY + step)
                            else (eX,eY)
          nearBombs = intersect bc (aroundCelld3 ecc field)
          (fbX,fbY) = head nearBombs
          (d2x,d2y) = (fbX - eX, fbY - eY)
          backWayCC = if abs d2x > abs d2y
                      then if d2x < 0
                           then if freeCell (eX + step, eY) field
                                then (eX + step, eY)
                                else (eX,eY)
                           else if freeCell (eX - step, eY) field
                                then (eX - step, eY)
                                else (eX,eY)
                      else if d2y <= 0
                           then if freeCell (eX, eY + step) field
                                then (eX, eY + step)
                                else (eX,eY)
                           else if freeCell (eX, eY - step) field
                                then (eX, eY - step)
                                else (eX,eY)
