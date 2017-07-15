{-# LANGUAGE Trustworthy #-}

module Dungeon (
    Terrain(..),
    Dungeon(..),
    boxDungeon,
    rogueDungeon,
    getTerrainAt
    ) where


-- base
import           Control.Monad              (forM, forM_)
import           Control.Monad.ST
--vector
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
-- random
import           System.Random              (RandomGen)
-- MonadRandom
import           Control.Monad.Random.Class (MonadRandom, getRandomR,
                                             uniformMay)
import           Control.Monad.Random.Lazy  (runRandT)
-- transformers
import           Control.Monad.Trans.Class
-- primitive
import           Control.Monad.Primitive    (PrimMonad, PrimState)
-- containers
import           Data.Set                   (Set)
import qualified Data.Set                   as S


-- | The terrain types of the dungeon.
data Terrain = Open -- ^ Space you can walk and see though
             | Wall -- ^ Rock walls you can't walk or see though
             | StairsDown -- ^ Stairs deeper into the dungeon
             | StairsUp -- ^ Stairs back to the surface.
             deriving (Read, Show)


-- | A single floor of the complete dungeon complex.
data Dungeon = Dungeon {
    dungeonWidth  :: Int,
    dungeonHeight :: Int,
    dungeonTiles  :: Vector Terrain
    } deriving (Read, Show)

-- | Determines the terrain at the location specified. An out of bounds location
-- defaults to being a 'Wall' value.
getTerrainAt :: (Int,Int) -> Dungeon -> Terrain
getTerrainAt (x,y) d = let
    width = dungeonWidth d
    height = dungeonHeight d
    index = y * width + x
    in if y < 0 || x < 0 || y >= height || x >= width
        then Wall
        else V.unsafeIndex (dungeonTiles d) index


-- | Constructs a dungeon with walls along the outer edges.
boxDungeon :: Int -> Int -> Dungeon
boxDungeon xMax yMax = Dungeon xMax yMax $
    V.generate (xMax*yMax) (\i -> let
        (y,x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
            then Wall
            else Open)

-- | A room during the rogueDungeon generation.
data Room = Room !Int !Int !Int !Int
    deriving (Show)


-- | Makes a room with the coordinates swapped around so that they're always in
-- the order xlow ylow xhigh yhigh
mkRoom :: (Int,Int) -> (Int,Int) -> Room
mkRoom (x1,y1) (x2,y2) = let
    xlow = min x1 x2
    ylow = min y1 y2
    xhigh = max x1 x2
    yhigh = max y1 y2
    in Room xlow ylow xhigh yhigh

-- | A hall that connects two rooms.
type Hall = Room -- Note: we might make these more distinct later

-- | If two rooms are vertically or horizontally oriented with one another.
data Orientation = Vertical | Horizontal deriving Show

-- | Sets a box area of the vector to be the value given.
setBox :: PrimMonad m =>
    Int -> VM.MVector (PrimState m) a -> (Int,Int) -> (Int,Int) -> a -> m ()
setBox width vec (x1,y1) (x2,y2) tile = do
    let xmin = min x1 x2
        xmax = max x1 x2
        ymin = min y1 y2
        ymax = max y1 y2
        targets = do
            y <- [ymin .. ymax]
            x <- [xmin .. xmax]
            pure $ width*y+x
    mapM_ (\i -> VM.write vec i tile) targets


-- | Turns the room given into open space.
drawRoom :: PrimMonad m =>
    Int -> VM.MVector (PrimState m) Terrain -> Room -> m ()
drawRoom width vec (Room xlow ylow xhigh yhigh) =
    setBox width vec (xlow,ylow) (xhigh,yhigh) Open

-- | Turns the hallway given into open space.
drawHall :: PrimMonad m =>
    Int -> VM.MVector (PrimState m) Terrain -> Hall -> m ()
drawHall = drawRoom -- Note: This might be different later

-- | Creates a dungeon where the rooms are all arranged in a 3x3 grid. Only
-- works well when the dungeon size is close to a terminal's normal size. The
-- two Int arguments are width and height (in that order).
rogueDungeon :: RandomGen g => Int -> Int -> g -> (Dungeon,g)
rogueDungeon width height g = let
    tileCount = width*height
    secWidth = width `div` 3
    secHeight = height `div` 3
    (tileVector, gFinal) = runST $ flip runRandT g $ do
        vec <- VM.replicate tileCount Wall
        -- Decide our room locations one sector at a time. Because every sector
        -- needs little offsets to avoid rooms touching on their own, this code
        -- is unfortunately kinda ugly no matter how you try to arrange or
        -- collapse it, so we'll just have to live with it like this.
        rooms <- sequence [
            randRoom 1 (secWidth-1) 1 (secHeight-1),
            randRoom (secWidth+1) (2*secWidth-1) 1 (secHeight-1),
            randRoom (2*secWidth+1) (width-2) 1 (secHeight-1),
            randRoom 1 (secWidth-1) (secHeight+1) (2*secHeight-1),
            randRoom (secWidth+1) (2*secWidth-1) (secHeight+1) (2*secHeight-1),
            randRoom (2*secWidth+1) (width-2) (secHeight+1) (2*secHeight-1),
            randRoom 1 (secWidth-1) (2*secHeight+1) (height-2),
            randRoom (secWidth+1) (2*secWidth-1) (2*secHeight+1) (height-2),
            randRoom (2*secWidth+1) (width-2) (2*secHeight+1) (height-2)
            ]
        -- draw the rooms
        mapM_ (drawRoom width vec) rooms
        -- do all of the connections
        forM_ [1..12] $ \borderIndex -> do
            let (room1, room2, orientation) = case borderIndex of
                    1  -> (rooms!!0, rooms!!1, Horizontal)
                    2  -> (rooms!!1, rooms!!2, Horizontal)
                    3  -> (rooms!!3, rooms!!4, Horizontal)
                    4  -> (rooms!!4, rooms!!5, Horizontal)
                    5  -> (rooms!!6, rooms!!7, Horizontal)
                    6  -> (rooms!!7, rooms!!8, Horizontal)
                    7  -> (rooms!!0, rooms!!3, Vertical)
                    8  -> (rooms!!3, rooms!!6, Vertical)
                    9  -> (rooms!!1, rooms!!4, Vertical)
                    10 -> (rooms!!4, rooms!!7, Vertical)
                    11 -> (rooms!!2, rooms!!5, Vertical)
                    12 -> (rooms!!5, rooms!!8, Vertical)
            -- if rooms aren't lined up we might have to draw more than one hall
            -- to connect two rooms, so we get them back as a list and then map
            -- over that list.
            halls <- pickHallways room1 room2 orientation
            mapM_ (drawHall width vec) halls
        -- "No need for an extra copy, Gold Leader. I'm already on my way out."
        V.unsafeFreeze vec
    in (Dungeon width height tileVector, gFinal)


-- | Given xlow xhigh ylow yhigh, generates a random room value that fits within
-- the bounds.
randRoom :: (MonadRandom m) => Int -> Int -> Int -> Int -> m Room
randRoom xlow xhigh ylow yhigh = do
    x1 <- getRandomR (xlow,xhigh)
    y1 <- getRandomR (ylow,yhigh)
    x2 <- getRandomR (xlow,xhigh)
    y2 <- getRandomR (ylow,yhigh)
    pure $ mkRoom (x1,y1) (x2,y2)

-- | The set of X coordinate values that overlap between the two rooms given.
overlapX :: Room -> Room -> Set Int
overlapX (Room r1xl _ r1xh _) (Room r2xl _ r2xh _) =
    S.intersection
        (S.fromAscList [r1xl .. r1xh])
        (S.fromAscList [r2xl .. r2xh])

-- | The set of Y coordinate values that overlap between the two rooms given.
overlapY :: Room -> Room -> Set Int
overlapY (Room _ r1yl _ r1yh) (Room _ r2yl _ r2yh) =
    S.intersection
        (S.fromAscList [r1yl .. r1yh])
        (S.fromAscList [r2yl .. r2yh])

-- | Given two room bounds, and a hint about if they're vertically oriented with
-- each other or not, we return the list of hallways that should be drawn to
-- connect the rooms.
pickHallways :: MonadRandom m => Room -> Room -> Orientation -> m [Hall]
pickHallways r1@(Room r1xl r1yl r1xh r1yh) r2@(Room r2xl r2yl r2xh r2yh) Vertical = do
    mayX <- uniformMay (overlapX r1 r2)
    case mayX of
        Just x -> pure [mkRoom (x,min r1yl r2yl) (x,max r1yh r2yh)]
        Nothing -> do
            -- there's no overlap, we need to make a turn-hallway
            r1x <- getRandomR (r1xl, r1xh)
            r2x <- getRandomR (r2xl, r2xh)
            let topOfLowerRoom = min r1yh r2yh
                bottomOfUpperRoom = max r2yl r2yh
            crossY <- getRandomR (topOfLowerRoom, bottomOfUpperRoom)
            pure $ [
                mkRoom (r1x,min crossY r1yl) (r1x,max crossY r1yh), -- seg1
                mkRoom (r1x,crossY) (r2x,crossY), -- cross
                mkRoom (r2x,min crossY r2yl) (r2x,max crossY r2yh)-- seg2
                ]
pickHallways r1@(Room r1xl r1yl r1xh r1yh) r2@(Room r2xl r2yl r2xh r2yh) Horizontal = do
    mayY <- uniformMay (overlapY r1 r2)
    case mayY of
        Just y -> pure [mkRoom (min r1xl r2xl,y) (max r1xh r2xh,y)]
        Nothing -> do
            -- there's no overlap, we need to make a turn-hallway
            r1y <- getRandomR (r1yl, r1yh)
            r2y <- getRandomR (r2yl, r2yh)
            let rightOfLowerRoom = min r1xh r2xh
                leftOfUpperRoom = max r2xl r2xh
            crossX <- getRandomR (rightOfLowerRoom, leftOfUpperRoom)
            pure $ [
                mkRoom (min crossX r1xl,r1y) (max crossX r1xl,r1y), -- seg1
                mkRoom (crossX,r1y) (crossX,r2y), -- cross
                mkRoom (min crossX r2xl,r2y) (max crossX r2xl,r2y)-- seg2
                ]
