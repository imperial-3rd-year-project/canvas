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
    state' <- handleEvent context state
    loop context state'

handleEvent :: DeviceContext -> CanvasState -> IO CanvasState
handleEvent context state@(CanvasState (l:ls) mouseState)= do 
    event <- wait context
    send context $ case (eType event, ePageXY event) of 
        ("mousedown", Just (x,y)) -> do 
            save()
            moveTo(x,y)
            restore()
            return $ CanvasState (((x,y):l):ls) MouseDown
        ("mouseup", Just (x,y)) -> return $ CanvasState ([]:l:ls) MouseUp
        ("mousemove", Just (x,y)) -> 
            if (mouseState == MouseUp) 
                then return $ state 
                else do 
                    save()
                    lineTo(x,y)
                    lineWidth 10
                    strokeStyle "red"
                    stroke() 
                    restore()
                    return $ (CanvasState (((x,y):l):ls) mouseState)
        _ -> return $ state




