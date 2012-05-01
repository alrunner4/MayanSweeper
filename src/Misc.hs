
module Misc where

import Data.Monoid
import Graphics.Rendering.OpenGL as OpenGL

---------
-- * Math
---------

-- | divide a 'Real' number by a 'Fractional' number
i `idivf` f = realToFrac i / f
infixl 7 `idivf`

-- | divide 'Real' numbers into a 'Fractional' result
l `divf` r = realToFrac l / realToFrac r
infixl 7 `divf`

-- | multiply 'Real' numbers into a 'Floating' result
l `mulf` r = realToFrac l * realToFrac r
infixl 7 `mulf`

clamp min max v = if v < min then min else if v > max then max else v

clamp01 :: ( Ord n , Num n ) => n -> n
clamp01 = clamp 0 1


---------------------
-- * Data Convenience
---------------------

justOrDefault _ ( Just v ) = v
justOrDefault d ( Nothing ) = d

justOrFalse = justOrDefault False

-----------
-- * OpenGL
-----------

vert2d :: Double -> Double -> Vertex2 GLdouble
vert2d x y = Vertex2 ( realToFrac x ) ( realToFrac y )

vert3d :: Double -> Double -> Double -> Vertex3 GLdouble
vert3d x y z = Vertex3 ( realToFrac x ) ( realToFrac y ) ( realToFrac z )

vert2i :: Int -> Int -> Vertex2 GLint
vert2i x y = Vertex2 ( fromIntegral x ) ( fromIntegral y )

color3d :: Double -> Double -> Double -> Color3 GLdouble
color3d r g b = Color3 ( realToFrac r ) ( realToFrac g ) ( realToFrac b )

renderFilledRect :: ( Color c , VertexComponent v ) => c -> Vertex2 v -> Vertex2 v -> IO ()
renderFilledRect c ( Vertex2 x1 y1 ) ( Vertex2 x2 y2 ) = renderPrimitive Quads $ do
  color c
  vertex $ Vertex2 x1 y1
  vertex $ Vertex2 x2 y1
  vertex $ Vertex2 x2 y2
  vertex $ Vertex2 x1 y2

renderOutlineRect :: ( Color c , VertexComponent v ) => c -> Vertex2 v -> Vertex2 v -> IO ()
renderOutlineRect c ( Vertex2 x1 y1 ) ( Vertex2 x2 y2 ) = renderPrimitive LineLoop $ do
  color c
  vertex $ Vertex2 x1 y1
  vertex $ Vertex2 x2 y1
  vertex $ Vertex2 x2 y2
  vertex $ Vertex2 x1 y2

usingMatrix m f = preservingMatrix $ do multMatrix m ; f
