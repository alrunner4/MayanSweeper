
module Minesweeper where

----------------------
-- External Imports --
----------------------
import Data.Fixed ( mod' )
import Data.Time.Clock
import Graphics.Rendering.OpenGL as OpenGL
import System.Random ( RandomGen ( next ), getStdGen )

import qualified Graphics.UI.GLFW as GLFW

---------------------
-- Project Imports --
---------------------
import Display
import Grid
import Input
import Logic
import Misc
import Numerals

import Debug.Trace ( trace, traceIO )

--------------------
-- Initialization --
--------------------
main = do
	
	GLFW.initialize
	
	GLFW.openWindow GLFW.defaultDisplayOptions {
		GLFW.displayOptions_width = 512,
		GLFW.displayOptions_height = 512,
		GLFW.displayOptions_windowIsResizable = False
	}
	
	GLFW.setWindowBufferSwapInterval 1
	GLFW.setWindowSizeCallback resize
	
	stdRand <- getStdGen
	now <- getCurrentTime
	
	OpenGL.clearColor $= Color4 0.5 0.5 0.5 1
	clear [ColorBuffer]
	
	initialInput <- noInput
	
	mainLoop ( initialGameState stdRand now , initialInput )
	
	GLFW.terminate

---------------
-- Main Loop --
---------------
mainLoop :: ( GameState , InputState ) -> IO GameState
mainLoop ( gameState , prevInputs ) = do
	
	GLFW.swapBuffers
	clear [ColorBuffer]
	
	escape <- GLFW.keyIsPressed GLFW.KeyEsc
	windowOpen <- GLFW.windowIsOpen
	
	let gridDimension = gridSize $ mineGrid gameState
	
	inputs <- captureInputs prevInputs gridDimension
	now <- getCurrentTime
	
	let gameState' = if newGameRequested inputs
		then
			initialGameState ( rand inputs ) now
		else
			advanceGameState gameState inputs
		
	display gameState' inputs
	
	if escape || not windowOpen
		then return gameState'
		else mainLoop ( gameState' , inputs )

---------------
-- CALLBACKS --
---------------
resize :: GLFW.WindowSizeCallback
resize w h = let
	wf = ( realToFrac w ) :: Double
	w' = toEnum w
	hf = ( realToFrac h ) :: Double
	h' = toEnum ( truncate ( hf * aspect ) )
	aspect = wf / hf
	in do viewport $= ( Position 0 0 , Size w' h' )

