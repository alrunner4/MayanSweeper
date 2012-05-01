
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
    advanceGameState
  )
  where

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
                            | ( not . won ) game && gameIsWon = game { won = True }
                            | won game         = doNothing
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
    
    mineIsSelected = case maybeLocation of
                       Nothing -> False
                       Just loc -> isMine loc
    
    flagIsSelected = case maybeLocation of
                       Nothing -> False
                       Just loc -> isFlagged loc
    
    clearIsSelected = case maybeLocation of
                        Nothing -> False
                        Just loc -> isCleared loc
    
    recursiveClear :: MineGrid -> ( Int , Int ) -> MineGrid
    recursiveClear g pos = case stateForLocation grid pos of
                           Nothing -> g
                           Just loc -> if g `minesNeighboringLocation` pos == 0
                             then
                               foldr ($) ( g `clearLocation` pos ) toClearFuncs
                             else
                               g `clearLocation` pos
      where
        toClear = filter ( \ ( _ , l ) -> ( not $ isCleared l ) && ( not $ isFlagged l ) ) ( neighborsOfLocation g pos )
        toClearFuncs = map ( \ ( p , _ ) -> ( `recursiveClear` p ) ) toClear
    
    -- recursiveClearMultiple g [] = g
    -- recursiveClearMultiple g ( p : ps ) = case stateForLocation grid p of
    --   Nothing -> recursiveClearMultiple g ps
    --   Just loc -> ( g `clearLocation` p ) `recursiveClearMultiple` 
    --   where
    --     toClear = filter ( not . isCleared . snd ) ( neighborsOfLocation g pos )
    
    gameIsWon = unclearedLeft grid == 0
    
    -------------
    -- Actions --
    -------------
    
    doNothing = game
    
    gameAfterClick = if mineIsSelected && not flagIsSelected then explode game else
                     if flagIsSelected then doNothing else
                        game { mineGrid = grid `recursiveClear` selectedPos }
    
    gameAfterRightClick = if not clearIsSelected then toggleFlag selectedPos else doNothing

