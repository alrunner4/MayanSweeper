
module Numerals
  (
    renderNumeral
  )
  where

import Graphics.Rendering.OpenGL as OpenGL
import Misc

exteriorPadding = 0.1 :: Double
interiorPadding = 0.1 :: Double
glyphSize = ( 1 - 2 * exteriorPadding - 3 * interiorPadding ) / 4

glyphColor = color3d 1 1 1

renderGlyphRect ( ( b , l ) , ( t , r ) ) = renderFilledRect glyphColor ( vert2d b l ) ( vert2d t r )

-- | @renderNumeral@ renders a base-20, Mayan-style numeral in the OpenGL rectangle with corners @((0,0), (1,1))@.

renderNumeral :: ( Integral i , Show i ) => i -> IO ()
renderNumeral n | n < 0 || n > 20 = return ()
                | otherwise = sequence_ $ map renderGlyphRect ( numeralLayout n )

-- intervals for a row of glyphs, left-to-right
rowLayout :: ( Integral i , Show i ) => i -> [ ( Double , Double ) ]
rowLayout n | n > 0 && n < 5 = map interval [1..n]
            | n == 5 = [ ( exteriorPadding , 1 - exteriorPadding ) ]
            | otherwise = []
  where
    margin :: Double
    margin = ( 1 - glyphSize `mulf` n - interiorPadding `mulf` ( n - 1 ) ) / 2
    left i = margin + ( i - 1 ) `mulf` ( glyphSize + interiorPadding )
    right i = left i + glyphSize
    interval :: Real i => i -> ( Double , Double )
    interval i = ( left i , right i )

-- intervals for all rows of glyphs, bottom-to-top
-- numeralLayout :: Integral i => i -> [ ( ( Double , Double ) , [ ( Double , Double ) ] ) ]
numeralLayout n | n > 0 && n < 20 = concat $ zipWith interval2d verticalIntervals rowIntervals
                | otherwise = []
  where
    numVerticalIntervals = ceiling $ n `divf` 5
    margin =  ( 1 - glyphSize `mulf` numVerticalIntervals - interiorPadding `mulf` ( numVerticalIntervals - 1 ) ) / 2
    bottom i = margin + ( i - 1 ) `mulf` ( glyphSize + interiorPadding )
    top i = bottom i + glyphSize
    interval :: Real r => r -> ( Double , Double )
    interval i = ( bottom i , top i )
    verticalIntervals = map interval [1..numVerticalIntervals]
    rowValues :: Integral i => i -> [ i ]
    rowValues x = if x > 5 then 5 : rowValues ( x - 5 ) else [x]
    rowIntervals = map rowLayout ( rowValues n )
    
    -- interval2d :: ( a , b ) -> [ ( c , d ) ] -> [ ( ( a , c ) , ( b , d ) ) ]
    interval2d ( b , t ) ( ( l , r ) : rem ) = ( ( l , b ) , ( r , t ) ) : interval2d ( b , t ) rem
    interval2d _ [] = []
