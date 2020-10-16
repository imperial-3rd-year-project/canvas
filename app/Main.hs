{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Graphics.Blank
import Data.Text

type Coord = (Double, Double)

type Line = [Coord]

data MouseState = MouseUp | MouseDown deriving Eq

main :: IO ()
main = blankCanvas 3000 
                   { events = ["mousedown", "mouseup", "mousemove"] } 
                   (\context -> loop context MouseUp)

loop :: DeviceContext -> MouseState -> IO ()
loop context state = do 
  state' <- handleEvent context state
  loop context state'

handleEvent :: DeviceContext -> MouseState -> IO MouseState
handleEvent context mouseState= do 
  event <- wait context
  send context $ case (eType event, ePageXY event, mouseState) of
    ("mousedown", Just (x, y), _) -> do
      save ()
      moveTo (x, y)
      restore ()
      return $ MouseDown
    
    ("mousemove", Just (x,y), MouseDown) -> do
      save ()
      lineTo (x, y)
      lineWidth 10
      strokeStyle "black"
      lineCap RoundCap
      lineJoin RoundCorner
      stroke ()
      restore ()
      return $ mouseState

    ("mouseup", Just (x,y), _) -> return $ MouseUp

    _ -> return $ mouseState




