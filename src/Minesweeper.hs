
module Minesweeper where

----------------------
-- External Imports --
----------------------
import Data.Fixed ( mod' )
import Data.Time.Clock
import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.GLFW
import System.Random ( RandomGen ( next ), getStdGen )

---------------------
-- Project Imports --
---------------------
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
  
  initialize
  
  openWindow defaultDisplayOptions { displayOptions_width = 512,
                                     displayOptions_height = 512,
                                     displayOptions_windowIsResizable = False }
  
  setWindowBufferSwapInterval 1
  setWindowSizeCallback resize
  
  stdRand <- getStdGen
  now <- getCurrentTime
  
  OpenGL.clearColor $= Color4 0.5 0.5 0.5 1
  clear [ColorBuffer]
  
  initialInput <- noInput
  
  mainLoop ( initialGameState stdRand now , initialInput )
  
  terminate

---------------
-- Main Loop --
---------------
mainLoop :: ( GameState , InputState ) -> IO GameState
mainLoop ( gameState , prevInputs ) = do
  
  swapBuffers
  clear [ColorBuffer]
  
  escape <- keyIsPressed KeyEsc
  windowOpen <- windowIsOpen
  
  let gridDimension = gridSize $ mineGrid gameState
  
  inputs <- captureInputs prevInputs gridDimension
  now <- getCurrentTime
  
  let gameState' = if newGameRequested inputs then initialGameState ( snd $ next $ rand inputs ) now else
                      advanceGameState gameState inputs
  
  let inputs' = if newGameRequested inputs then inputs { rand = snd $ next $ rand inputs } else inputs
  
  display gameState' inputs
  
  if escape || not windowOpen
    then return gameState'
    else mainLoop ( gameState' , inputs' )

-------------
-- Display --
-------------
display :: GameState -> InputState -> IO ()
display game input =

  let
    mineColor = color3d 1 0 0
    flagColor = color3d 0 1 0
    clearColor = color3d 0 0 1
    defaultColor = color3d 0 0 0
    incorrectFlagColor = color3d 1 1 0
    
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
                         if isMine loc then mineColor else
                         if isFlagged loc then incorrectFlagColor else
                         if isCleared loc then clearColor else
                            defaultColor
            renderMineCount = if ( not $ isCleared loc ) || ( isFlagged loc )
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


resize :: WindowSizeCallback
resize w h = let
  wf = ( realToFrac w ) :: Double
  w' = toEnum w
  hf = ( realToFrac h ) :: Double
  h' = toEnum ( truncate ( hf * aspect ) )
  aspect = wf / hf
  in do viewport $= ( Position 0 0 , Size w' h' )

