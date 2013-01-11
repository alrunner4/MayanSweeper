
module Display ( display ) where

import Graphics.Rendering.OpenGL ( GLdouble, Vector3 ( Vector3 ), preservingMatrix, scale, translate )
import Grid
import Input
import Logic
import Misc
import Numerals

------------
-- COLORS --
------------

clearColor = color3d 0 0 1
defaultColor = color3d 0 0 0
explodedColor = color3d 1 0 0
flagColor = color3d 0 1 0
incorrectFlagColor = color3d 1 1 0
mineColor = color3d 0.75 0 0

display :: GameState -> InputState -> IO ()
display game input = let

	( ( xMin , yMin ) , ( xMax , yMax ) ) = gridBounds $ mineGrid game
	( gridw , gridh ) = gridSize $ mineGrid game
	
	minew = 2 `divf` gridw
	mineh = 2 `divf` gridh

	coords = [ ( x , y ) | x <- [ xMin .. xMax ] , y <- [ yMin .. yMax ] ]
	
	renderLoc ( cx , cy ) =
		let
			mloc = stateForLocation ( mineGrid game ) ( cx , cy )
		in case mloc of
			Nothing  -> return ()
			Just loc -> do
				renderFilledRect locColor ( vert2d left bot ) ( vert2d right top )
				renderMineCount
				where
					left = ( cx - xMin ) `mulf` minew - 1
					right = left + minew
					bot = ( cy - yMin ) `mulf` mineh - 1
					top = bot + mineh

					locColor = if ( not $ won game ) && ( not $ exploded game ) then
						if isMine loc && exploded game then mineColor else
						if isFlagged loc then flagColor else
						if isCleared loc then clearColor else
							defaultColor
					 else
						if isMine loc && isFlagged loc then flagColor else
						if isMine loc && isCleared loc then explodedColor else
						if isMine loc then mineColor else
						if isFlagged loc then incorrectFlagColor else
						if isCleared loc then clearColor else
							defaultColor

					renderMineCount = if ( isMine loc ) || ( not $ isCleared loc ) || ( isFlagged loc )
						then return ()
						else preservingMatrix $ do
							 translate $ Vector3 ( realToFrac left ) ( realToFrac bot ) ( 0 :: GLdouble )
							 scale ( realToFrac minew ) ( realToFrac mineh ) ( 1 :: GLdouble )
							 renderNumeral $ minesNeighboringLocation ( mineGrid game ) ( cx , cy )

	renderCursor ( cx , cy ) = renderOutlineRect ( color3d 1 1 1 ) ( vert2d left bot ) ( vert2d right top )
		where
			left = realToFrac ( cx - xMin ) * minew - 1 + 0.1 * minew
			right = left + 0.8 * minew
			bot = realToFrac ( cy - yMin ) * mineh - 1 + 0.1 * mineh
			top = bot + 0.8 * mineh
	
	in do
		sequence_ $ map renderLoc coords
		renderCursor ( xCellMouse input , yCellMouse input )
		-- putStrLn $ "dt = " ++ show ( diffTime input ) ++ " (" ++ show ( floor $ recip $ toRational $ diffTime input ) ++ "Hz)"

