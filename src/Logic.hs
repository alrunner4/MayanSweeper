
module Logic
  (
    -- * GameState
    GameState,
    -- ** Queries
    mineGrid,
    gameTime,
    exploded,
    won,
    -- ** Construction
    initialGameState,
    -- ** Transformation
    advanceGameState
  )
  where

import Data.Maybe ( mapMaybe )
import Data.Time.Clock
import Grid
import Input
import System.Random ( RandomGen )

----------------
-- Game State --
----------------
data GameState = GameState
  {
    mineGrid :: MineGrid,
    gameTime :: UTCTime,
    exploded :: Bool,
    won :: Bool
  }

initialGameState :: RandomGen g => g -> UTCTime -> GameState
initialGameState gen t = GameState
  {
    mineGrid = randomMineGrid gen 0.15 21 21,
    gameTime = t,
    exploded = False,
    won = False
  }

advanceGameState :: GameState -> InputState -> GameState
advanceGameState game input | exploded game    = doNothing
                            | won game         = doNothing
                            | gameIsWon        = game { won = True }
                            | lmbClicked input = gameAfterClick { gameTime = currentTime input }
                            | rmbClicked input = gameAfterRightClick { gameTime = currentTime input }
                            | otherwise        = game { gameTime = currentTime input }
  where
    
    grid = mineGrid game
    
    explode g = g { exploded = True }
    
    transformGridBy f = game { mineGrid = f grid }
    
    g `transformBy` f = f g
    
    toggleFlag l = transformGridBy ( `toggleFlagLocation` l )
    
    clearLoc l = transformGridBy ( `clearLocation` l )
    
    selectedPos = ( xCellMouse input , yCellMouse input )
    
    maybeLocation = stateForLocation grid selectedPos
        
    flagIsSelected = case maybeLocation of
      Nothing -> False
      Just loc -> isFlagged loc
    
    clearIsSelected = case maybeLocation of
      Nothing -> False
      Just loc -> isCleared loc
    
    doClear :: GameState -> ( Int , Int ) -> GameState
    doClear g pos = case ( mineGrid g ) `stateForLocation` pos of
      Nothing -> g
      Just loc -> if isMine loc
        then
          explode $ clearLoc pos
        else if ( mineGrid g ) `minesNeighboringLocation` pos == 0
        then
          g `recursiveClear` pos
        else
          g { mineGrid = mineGrid g `clearLocation` pos }

    recursiveClear :: GameState -> ( Int , Int ) -> GameState
    recursiveClear g pos = case stateForLocation ( mineGrid g ) pos of
      Nothing -> g
      Just loc -> foldr ($) g { mineGrid = mineGrid g `clearLocation` pos } toClearFuncs
      where
        toClear = filter ( \ ( _ , l ) -> ( not $ isCleared l ) && ( not $ isFlagged l ) ) ( neighborsOfLocation ( mineGrid g ) pos )
        toClearFuncs = map ( \ ( p , _ ) -> ( `doClear` p ) ) toClear
    
    adjacentClear :: GameState -> ( Int , Int ) -> GameState
    adjacentClear g pos = case stateForLocation ( mineGrid g ) pos of
      Nothing -> g
      Just loc -> foldr ($) g toClearFuncs
        where
          neighbors = mineGrid g `neighborsOfLocation` pos
          toClear = mapMaybe ( \ ( p , s ) -> if isCleared s then Nothing else if isFlagged s then Nothing else Just p ) neighbors
          toClearFuncs = map ( \ p -> ( `doClear` p ) ) toClear
    
    gameIsWon = unclearedLeft grid == 0
    
    -------------
    -- Actions --
    -------------
    
    doNothing = game
    
    gameAfterClick =
      if flagIsSelected
        then
          doNothing
        else
          game `doClear` selectedPos
    
    gameAfterRightClick =
      if not clearIsSelected
        then
          toggleFlag selectedPos
        else
          game `adjacentClear` selectedPos

