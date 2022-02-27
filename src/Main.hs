module Main where

import Graphics.WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad

-- | Gather list of all rectangles that can fit in the grid of size `col x rows`.
-- | Each element is a tuple of the form (x, y, rectangleWidth, rectangleHeight).
-- | E.g., for a `1 x 3` grid, this will produce a list of 6 values:
-- | @[(0,0,1,1),(0,1,1,1),(0,2,1,1),(0,0,1,2),(0,1,1,2),(0,0,1,3)]@
positionPermutations :: Int -> Int -> [(Int, Int, Int, Int)]
positionPermutations cols rows = [ (x, y, width, height) 
                                 | height <- [1..rows] -- all heights
                                 , y <- [0..rows] -- all y positions
                                 , width <- [1..cols] -- all widths
                                 , x <- [0..cols] -- all x positions  
                                 , x + width <= cols -- filter rectangles: fit board width
                                 , y + height <= rows -- filter rectangles: fit board height
                                 ]

main :: IO ()
main = do 
    -- Change these values to change the number of rows x columsn in the grid.
    let cols = 3
    let rows = 2
    -- Run the turtle animation.
    runTurtle $ do
        -- Give turtle instant turns
        setRotationSpeed 0
        -- Set initial pen size and draw speed.
        setPenSize 5
        setSpeed 300
        -- Draw all rectangle permutations
        forM_ (positionPermutations cols rows) $ \ rectangle -> do
            -- Draw background grid
            drawGrid cols rows
             -- Draw this rectangle permutation.
            branch $ drawRectangle rectangle
        -- hide turtle at the end
        setVisible False

-- Side of 1 square in the grid (for drawing purposes).
sideLength :: Float
sideLength = 50

-- Given a tuple of form @(x, y, width, height)@, draws a rectangle
-- of size `width x height`, at position @(x, y)@.
drawRectangle:: (Int, Int, Int, Int) -> TurtleCommand ()
drawRectangle (x, y, width, height) = do
    -- Change pen color for next rectangle to match the gattengo color
    -- for a given width.
    setPenColor $ gattegnoColors !! (width - 1)
    -- Change turtle's color to match pen color.
    penColor >>= \c -> setRepresentation (G.color c $ G.circleSolid 10)
    -- Jump to (x, y) coordinate
    setPenDown False
    goto (sideLength * fromIntegral x, sideLength * fromIntegral y)
    -- Draw rectangle of size (width, height)
    setPenDown True
    setHeading east
    let w = sideLength * fromIntegral width 
    let h = sideLength * fromIntegral height
    forward w
    setHeading north
    forward h
    setHeading west
    forward w
    setHeading south
    forward h 

-- Draws the `cols x rows` grid onto the screen.
drawGrid :: Int -> Int -> TurtleCommand ()
drawGrid cols rows = branch $ do
    -- Ensure lines drawn instantly with no
    -- animation.
    setSpeed 0
    setPenDown False
    setVisible False
    setPenColor $ greyN 0.85
    goto (0, 0)
    -- Draw all the columns.
    branch $ forM_ [0..cols + 1] $ \x -> do
        branch $ do
            setHeading north
            setPenDown True
            forward $ sideLength * fromIntegral rows
        goto (sideLength * fromIntegral x, 0)
    -- Draw all the rows.
    branch $ forM_ [0..rows + 1] $ \y -> do
            branch $ do
                setHeading east
                setPenDown True
                forward $ sideLength * fromIntegral cols
            goto (0, sideLength * fromIntegral y)

-- | Color values use for rectangle drawing.
offWhite, lightGreen, purple, darkGreen, brown :: Color
offWhite = makeColorI 163 191 186 255
lightGreen = makeColorI 144 238 144 255
purple = makeColorI 128 0 128 255
darkGreen = makeColorI 1 50 32 255
brown = makeColorI 210 180 140 255

-- Color values associated with a specific index.
gattegnoColors  :: [Color]
gattegnoColors = cycle
               [ offWhite -- rods of length 1 are white
               , red -- length 2 are red
               , lightGreen -- 3, light green
               , purple -- 4
               , yellow -- 5
               , darkGreen -- 6
               , black -- 7
               , brown -- 8
               , blue -- 9
               , orange -- 10
               ] --- .... and cycle rod colors if we grow beyond this.