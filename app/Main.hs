module Main where

import Data
import Game
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  world <- initWorld
  play (InWindow
         "BombSquad"
         (windowWidth,windowHeight)
         (100,100)
        ) 
        (bgColor)
        fps
        world
        worldPic
        handleEvents
        updateScreen
