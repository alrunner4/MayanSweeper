
module Grid
  (
    -- * MineGrid
    MineGrid,
    
    -- ** Construction
    emptyMineGrid,
    randomMineGrid,
    
    -- ** Accessors
    gridBounds,
    gridSize,
    stateForLocation,
    minesNeighboringLocation,
    neighborsOfLocation,
    unclearedLeft,
    
    -- ** Transformations
    flagLocation,
    unflagLocation,
    toggleFlagLocation,
    clearLocation,
    
    -- * LocationState
    LocationState,
    
    -- ** Predicates
    isMine,
    isFlagged,
    isCleared
  )
  where

import Data.Array
import Data.Maybe ( catMaybes, mapMaybe )
import Data.List ( length )
import System.Random ( RandomGen, randomRs )

--------------
-- MineGrid --
--------------

newtype MineGrid = MineGrid { arrayForGrid :: Array ( Int , Int ) LocationState }


-- | @emptyMineGrid@ will always snap the dimensions of the generated grid to odd numbers so that there is always a center square at coordinate (0,0).
--   Even-numbered dimensions will automatically be increased by one to meet this constraint.

emptyMineGrid :: Int -- ^ desired @width@ of the grid
              -> Int -- ^ desired @height@ of the grid
              -> MineGrid

emptyMineGrid w h = MineGrid $ array ( ( -halfw , -halfh ) , ( halfw , halfh ) ) [ ( (x,y) , newMine ) | x <- [ -halfw .. halfw ], y <- [ -halfh .. halfh ] ]
  where
    halfw = if even w then ( w `div` 2 ) else ( w-1 `div` 2 )
    halfh = if even h then ( h `div` 2 ) else ( h-1 `div` 2 )


-- | @randomMineGrid@ generates a random grid of mines using the supplied random number generator.
--   The second parameter is the probability of there being a mine at any given location.
--   
--   * If the probability given is below zero, the generated grid will contain no mines.
--   
--   * If the probability given is above one, the generated grid will be completely mined.

randomMineGrid :: RandomGen g
               => g      -- ^ random number generator
               -> Double -- ^ chance of mine [0, 1]
               -> Int    -- ^ desired @width@ of the grid
               -> Int    -- ^ desired @height@ of the grid
               -> MineGrid

randomMineGrid gen p w h = MineGrid $ array ( ( -halfw , -halfh ) , ( halfw , halfh ) ) contents
  where
    halfw = if even w then ( w `div` 2 ) else ( (w-1) `div` 2 )
    halfh = if even h then ( h `div` 2 ) else ( (h-1) `div` 2 )
    rands = take ( ( 2 * halfw + 1 ) * ( 2 * halfh + 1 ) ) $ randomRs ( 0.0 , 1.0 ) gen
    coords = [ ( x , y ) | x <- [ -halfw .. halfw ], y <- [ -halfh .. halfh ] ]
    randomMine r = if r < p then newMine else newNoMine
    contents = zipWith ( \ (x,y) r -> ( (x,y) , randomMine r ) ) coords rands

-- | @gridBounds@ returns the minimum and maximum coordinates of the 'MineGrid' in the form @((xMin, yMin), (xMax, yMax))@.
gridBounds ( MineGrid g ) = bounds g


-- | @gridSize@ returns the dimensions of the 'MineGrid' in the form @(width, height)@
gridSize g = ( xmax - xmin + 1 , ymax - ymin + 1 )
  where ( ( xmin , ymin ) , ( xmax , ymax ) ) = gridBounds g

-- | @stateForLocation@ safely accesses the state of a location in a 'MineGrid'.
stateForLocation g l = if not $ inRange ( gridBounds g ) l then Nothing
                                    else Just $ arrayForGrid g ! l

-- | @minesNeighboringLocation@ is the count of mines surrounding a location
minesNeighboringLocation g ( x , y ) = length mineNeighbors
  where
    neighbors = catMaybes $ map ( \ l -> stateForLocation g l ) ( neighborCells g ( x , y ) )
    mineNeighbors = filter isMine neighbors

-- | @neighborsOfLocation@ is the list of @(location, 'LocationState')@ pairs for all neighbors of a location
neighborsOfLocation g loc = mapMaybe filterExists ( neighborCells g loc )
  where
    filterExists l = case stateForLocation g l of
                       Nothing -> Nothing
                       Just s -> Just ( l , s )

unclearedLeft g = length $ filter ( \ loc -> ( not $ isCleared loc ) && ( not $ isMine loc ) ) ( elems $ arrayForGrid g )

neighborCells g ( x , y ) = [ ( x - 1 , y - 1 ) , ( x , y - 1 ) , ( x + 1 , y - 1 ) ,
                              ( x - 1 , y     ) ,                 ( x + 1 , y     ) ,
                              ( x - 1 , y + 1 ) , ( x , y + 1 ) , ( x + 1 , y + 1 ) ]

------------------------------
-- MineGrid Transformations --
------------------------------

alterLocation g l f =
  let
    old = stateForLocation g l
  in
    case old of
      Nothing -> g
      Just s  -> if not $ inRange ( gridBounds g ) l then g
                 else MineGrid $ arrayForGrid g // [ ( l , f s ) ]


flagLocation mg l = alterLocation mg l withFlag

unflagLocation mg l = alterLocation mg l withoutFlag

toggleFlagLocation mg l = alterLocation mg l withToggledFlag


clearLocation mg l = alterLocation mg l withCleared


-------------------
-- LocationState --
-------------------

-- | The components of a 'MineGrid' are of type @LocationState@.
newtype LocationState = LocationState ( Bool , Bool , Bool )

-- | @newMine@ creates a 'LocationState' for a mine.
newMine = LocationState ( True , False , False )

-- | @newNoMine@ creates a 'LocationState' without a mine.
newNoMine = LocationState ( False , False , False )


-------------------------------
-- LocationState Predicates --
-------------------------------

isMine ( LocationState ( m , _ , _ ) ) = m

isFlagged ( LocationState ( _ , f , _ ) ) = f

isCleared ( LocationState ( _ , _ , c ) ) = c


------------------------------
-- LocationState Transforms --
------------------------------

withFlag ( LocationState ( m , _ , c ) ) = LocationState ( m , True , c )

withoutFlag ( LocationState ( m , _ , c ) ) = LocationState ( m , False , c )

withToggledFlag ( LocationState ( m , f , c ) ) = LocationState ( m , not f , c )

withCleared ( LocationState ( m , f , _ ) ) = LocationState ( m , f , True )
