module Main where

import Lib
import Graphics.Blank
import Data.Text

main :: IO ()
main = blankCanvas 3000 { events = [pack "mousedown"] } $ \context -> loop context []

loop :: DeviceContext -> [(Double, Double)] -> IO ()
loop context [] = do 
    event <- wait context
    case ePageXY event of
        Nothing -> loop context []
        Just (x',y') -> loop context ((x' ,y'):[])
loop context coords@((x,y):_) = do 
    send context $ do
        let (w,h) = (width context, height context)
        let minwh = min w h
        save()
        translate (x,y)
        beginPath()
        arc(0, 0, minwh * 0.1, 0, 2 * pi, False)
        lineWidth 10
        strokeStyle (pack "#00a000")
        stroke()
        restore()
    event <- wait context
    case ePageXY event of
        Nothing -> loop context coords
        Just (x',y') -> loop context ((x' ,y'):coords)


