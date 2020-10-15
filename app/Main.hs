{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Graphics.Blank
import Data.Text

type Coord = (Double, Double)

type Line = [Coord]

data MouseState = MouseUp | MouseDown deriving Eq

data CanvasState = CanvasState [Line] MouseState

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown", "mouseup", "mousemove"] } $ \context -> loop context (CanvasState [[]] MouseUp)

loop :: DeviceContext -> CanvasState -> IO ()
loop context state@(CanvasState (l:ls) mouseState) = do 
    send context $ do 
        save()
        drawLine context l 
        restore()
    state' <- handleEvent context state
    loop context state'

handleEvent :: DeviceContext -> CanvasState -> IO CanvasState
handleEvent context state@(CanvasState (l:ls) mouseState)= do 
    event <- wait context
    return $ case (eType event, ePageXY event) of 
        ("mousedown", Just (x,y)) -> CanvasState (((x,y):l):ls) MouseDown
        ("mouseup", Just (x,y)) -> CanvasState ([]:l:ls) MouseUp
        ("mousemove", Just (x,y)) -> if (mouseState == MouseUp) 
                                        then state 
                                        else (CanvasState (((x,y):l):ls) mouseState)
        _ -> state


drawLine :: DeviceContext -> [Coord] -> Canvas ()
drawLine context [] = mempty
drawLine context ((x,y):cs) = do 
    moveTo (x,y)
    drawLine' context cs 

drawLine' :: DeviceContext -> [Coord] -> Canvas ()
drawLine' context [] = mempty
drawLine' context line@((x,y):cs) = do 
    lineTo(x,y)
    lineWidth 10
    strokeStyle "red"
    stroke() 
    drawLine context line


