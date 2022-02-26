module Main where

import Graphics.WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad

-- | Gathers list of all permutations of rectangle widths/heights. Going from:
-- | `(1, 1), (1, 2), ...` all the way up to `(maxWidth, maxHeight)`.
-- | E.g. for `maxWidth=1` and `maxHeight=3`, we will produce a list of 3 values:
-- | @[(1,1),(1,2),(1,3)]@.
rectanglePermutations :: Int -> Int -> [(Int, Int)]
rectanglePermutations maxWidth maxHeight = [(x, y) | y  <- [1..maxHeight], x  <- [1..maxWidth]]

-- | Gather list of all rectangles that can fit in the grid of size `col x rows`.
-- | Each element is a tuple of the form (x, y, rectangleWidth, rectangleHeight).
-- | E.g., for a `1 x 3` grid, this will produce a list of 6 values:
-- | @[(0,0,1,1),(0,1,1,1),(0,2,1,1),(0,0,1,2),(0,1,1,2),(0,0,1,3)]@
positionPermutations :: Int -> Int -> [(Int, Int, Int, Int)]
positionPermutations cols rows = 
    let rectangles = rectanglePermutations cols rows
     in [(x, y, width, height) | (width, height) <- rectangles -- all rectangle types
                               , y <- [0..rows] -- all y positions
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

        -- Set initial pen color and draw speed.
        setPenColor cyan
        setSpeed 300
        
        -- Draw all rectangle permutations
        forM_ (positionPermutations cols rows) $ \ rectangle -> do
            -- Change pen color for next rectangle.
            penColor >>= setPenColor . shiftHue 149

            -- Change turtle's color to match pen color.
            penColor >>= \c -> setRepresentation (G.color c $ G.circleSolid 10)

            -- Draw background grid
            drawGrid cols rows

             -- Draw this rectangle permutation.
            branch $ drawRectangle rectangle

-- Side of 1 square in the grid (for drawing purposes).
sideLength :: Float
sideLength = 50

-- Given a tuple of form @(x, y, width, height)@, draws a rectangle
-- of size `width x height`, at position @(x, y)@.
drawRectangle:: (Int, Int, Int, Int) -> TurtleCommand ()
drawRectangle (x, y, width, height) = do
    -- Jump to (x, y) coordinate
    setPenDown False
    goto (sideLength * fromIntegral x, sideLength * fromIntegral y)
    -- Draw rectangle of size (width, height)
    setPenDown True
    setHeading east 
    forward $ sideLength * fromIntegral width
    setHeading north
    forward $ sideLength * fromIntegral height
    setHeading west
    forward $ sideLength * fromIntegral width
    setHeading south
    forward $ sideLength * fromIntegral height   

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
