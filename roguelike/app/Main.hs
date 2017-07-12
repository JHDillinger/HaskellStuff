{-# LANGUAGE TemplateHaskell #-}

module Main where

-- base
import           Control.Monad          (when)
import           Data.Char
import           Data.IORef
import           System.Exit            (die)
-- bytestring
import           Data.ByteString        (ByteString)
-- JuicyPixels
import           Codec.Picture
-- transformers
import           Control.Monad.IO.Class
-- embed-file
import           Data.FileEmbed
-- vector
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
-- hexes
import           Hexes



-- | The data for our display system. It gets embedded into our binary during
-- the compilation process, so we won't have to distribute any support files

imageBytes :: ByteString
imageBytes = $(embedFile "font-data/FixedSysExcelsior.png")


-- | Converts any pixel that is a fully opaque green into a tranparent black
-- pixel instead. any other kind of pixel is unaffected
greenToAlpha :: PixelRGBA8 -> PixelRGBA8
greenToAlpha (PixelRGBA8 0 255 0 255) = PixelRGBA8 0 0 0 0
greenToAlpha p                        = p

-- | Holds the entire state of the game in a single big blob.
data GameState = GameState {
    playerPos :: (Int,Int),
    dungeon   :: Dungeon
    } deriving (Read, Show)

-- | Constructs a GameState with the player at 5,5
mkGameState :: Int -> Int -> GameState
mkGameState xMax yMax = GameState (5,5) (mkDungeon xMax yMax)

-- | The terrain types of the dungeon.
data Terrain = Open
             | Wall
             | StairsDown
             | StairsUp
             deriving (Read, Show)

-- | A single floor of the complete dungeon complex.
data Dungeon = Dungeon {
    dungeonWidth  :: Int,
    dungeonHeight :: Int,
    dungeonTiles  :: Vector Terrain
    } deriving (Read, Show)


-- | Constructs a dungeon with walls along the outer edges.
mkDungeon :: Int -> Int -> Dungeon
mkDungeon xMax yMax = Dungeon xMax yMax $
    V.generate (xMax*yMax) (\i -> let
        (y,x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
            then Wall
            else Open)


-- | Determines the terrain at the location speficied. An out of bounds location
-- defaults to being a Wall value.
getTerrainAt :: (Int,Int) -> Dungeon -> Terrain
getTerrainAt (x,y) d = let
    width = dungeonWidth d
    height = dungeonHeight d
    index = y * width + x
    in if y < 0 || x < 0 || y >= height || x >= width
        then Wall
        else V.unsafeIndex (dungeonTiles d) index


main :: IO ()
main = do
    baseImage <- pixelMap greenToAlpha <$> either die (pure . convertRGBA8) (decodeImage imageBytes)
    let rows = 24
        cols = 80
    gameRef <- newIORef $ mkGameState cols rows
    runHexes rows cols baseImage $ do
        setKeyCallback $ Just $ \key scanCode keyState modKeys -> liftIO $ do
            case keyState of
                KeyState'Released -> pure ()
                _                 -> modifyIORef gameRef (gameUpdate key)
        gameLoop gameRef


-- | Converts a key press into the appropriate game state update.
gameUpdate :: Key -> GameState -> GameState
gameUpdate key = case key of
    Key'Up    -> bumpPlayer North
    Key'Down  -> bumpPlayer South
    Key'Left  -> bumpPlayer West
    Key'Right -> bumpPlayer East
    _         -> id



-- | The directions you can move.
data Direction = North
               | South
               | East
               | West
               deriving Show

-- | Attempts to bump the player one step in the direction given. This doesn't
-- do combat since we don't have other creatures, but this should do combat code
-- in the future if there's a creature in the way.
bumpPlayer :: Direction -> GameState -> GameState
bumpPlayer dir game = let
    px = fst $ playerPos game
    py = snd $ playerPos game
    targetx = px + case dir of
        East -> 1
        West -> -1
        _    -> 0
    targety = py + case dir of
        North -> 1
        South -> -1
        _     -> 0
    in case getTerrainAt (targetx,targety) (dungeon game) of
        Wall -> game
        _    -> game { playerPos = (targetx,targety) }

-- | The core loop of the program. It needs the GameState in an IORef because
-- the GLFW events update the game state via callback.
gameLoop :: IORef GameState -> Hexes ()
gameLoop gameRef = do
    enterLoop <- not <$> windowShouldClose
    when enterLoop $ do
        -- update game state
        pollEvents
        -- draw to the screen
        gameState <- liftIO $ readIORef gameRef
        let playerID = fromIntegral $ ord '@'
            playerBG = V3 0 0 0
            playerFG = V4 1 1 1 1
            openID = fromIntegral $ ord ' '
            openBG = V3 0 0 0
            openFG = V4 0 0 0 0
            wallID = 1
            wallBG = V3 0 0 0
            wallFG = V4 0.388 0.152 0.027 1
            (px,py) = playerPos gameState
            terrainList = V.toList $ dungeonTiles $ dungeon gameState
            width = dungeonWidth $ dungeon gameState
            height = dungeonHeight $ dungeon gameState
            enumeratedTerrain = zip [0..] terrainList
            updateList = map (\(i,t) -> let
                (y',x) = i `divMod` width
                y = height - (1+y')
                in if px == x && py == y
                    then (playerID, playerBG, playerFG)
                    else case t of
                        Open -> (openID, openBG, openFG)
                        Wall -> (wallID, wallBG, wallFG)
                        _    -> (1, playerBG, playerFG)) enumeratedTerrain
        setAllByID updateList
        -- "blit"
        refresh
        -- loop
        gameLoop gameRef
