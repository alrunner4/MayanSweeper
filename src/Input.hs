
module Input
  (
    -- * InputState
    InputState,
    -- ** Construction
    captureInputs,
    noInput,
    -- ** Queries
    rand,
    -- *** Mouse
    xPosMouse,
    yPosMouse,
    xCellMouse,
    yCellMouse,
    -- lmb,
    -- rmb,
    lmbClicked,
    lmbDown,
    lmbWasDown,
    rmbClicked,
    rmbDown,
    rmbWasDown,
    -- *** Time
    currentTime,
    diffTime,
    -- ** Events
    newGameRequested,
    
    -- - ButtonState
    -- ButtonState,
    -- buttonDown,
    -- buttonWasDown,
    -- buttonClicked,
  )
  where

import Data.Time.Clock
import Graphics.UI.GLFW
import Grid
import Misc
import System.Random ( StdGen, getStdGen, split )

-----------------
-- Input State --
-----------------
data InputState = InputState
  {
    xPosMouse :: Double,
    yPosMouse :: Double,
    xCellMouse :: Int,
    yCellMouse :: Int,
    lmb :: ButtonState,
    rmb :: ButtonState,
    currentTime :: UTCTime,
    diffTime :: NominalDiffTime,
    newGame :: ButtonState,
    rand :: StdGen
  }

noInput = do
  t <- getCurrentTime
  r <- getStdGen
  return InputState
    {
      xPosMouse = 0,
      yPosMouse = 0,
      xCellMouse = 0,
      yCellMouse = 0,
      lmb = buttonStateClear,
      rmb = buttonStateClear,
      currentTime = t,
      diffTime = t `diffUTCTime` t,
      newGame = buttonStateClear,
      rand = r
    }

lmbDown = buttonDown . lmb
rmbDown = buttonDown . rmb

lmbWasDown = buttonWasDown . lmb
rmbWasDown = buttonWasDown . rmb

lmbClicked = buttonClicked . lmb
rmbClicked = buttonClicked . rmb

newGameRequested = buttonClicked . newGame

------------------
-- Button State --
------------------
data ButtonState = ButtonState
  {
    buttonDown :: Bool,
    buttonWasDown :: Bool
  }

buttonStateClear = ButtonState False False

buttonClicked ( ButtonState d wd ) = wd && not d

captureInputs :: ( Real w , Real h )
              => InputState -- ^ the previous input state
              -> ( w , h )  -- ^ @( width , height )@ of the mine grid
              -> IO InputState

captureInputs prev ( gridw , gridh ) = do

  ( xRawMouse , yRawMouse ) <- getMousePosition
  ( winw , winh ) <- getWindowDimensions
  leftMouse <- mouseButtonIsPressed MouseButton0
  rightMouse <- mouseButtonIsPressed MouseButton1
  now <- getCurrentTime
  newGameRequest <- keyIsPressed KeyF2
  
  let xMappedMouse = ( xRawMouse `divf` winw - 0.5 ) * 2
      yMappedMouse = ( negate $ yRawMouse `divf` winh - 0.5 ) * 2
  
  let newGameButtonState = ButtonState newGameRequest ( buttonDown $ newGame prev )
      isNewGameRequested = buttonClicked newGameButtonState

  return prev {
      xPosMouse = xMappedMouse,
      yPosMouse = yMappedMouse,
      xCellMouse = round $ xMappedMouse `mulf` gridw `divf` 2,
      yCellMouse = round $ yMappedMouse `mulf` gridh `divf` 2,
      lmb = ButtonState leftMouse ( lmbDown prev ),
      rmb = ButtonState rightMouse ( rmbDown prev ),
      currentTime = now,
      diffTime = now `diffUTCTime` currentTime prev,
      newGame = newGameButtonState,
      rand = if isNewGameRequested then ( fst $ split $ rand prev ) else rand prev
    }
