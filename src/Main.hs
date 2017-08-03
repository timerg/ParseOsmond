{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Parser.Parser
import Type.DataType

-- data Label = Imperial Double | Metric Double
-- type Point = (Label, Label)
-- data Path = Path Layer Trace [Point]

shiftPoint :: Unit -> (Double, Double) -> Point -> Point
shiftPoint MM (x, y) (lx, ly) = (addLabel lx x, addLabel ly y)
                    where addLabel (Imperial a) b = Imperial (a + b * 39.37)
                          addLabel (Metric a) b = Metric (a + b)
shiftPoint MIL (x, y) (lx, ly) = (addLabel lx x, addLabel ly y)
                    where addLabel (Imperial a) b = Imperial (a + b)
                          addLabel (Metric a) b = Imperial ((a + b) * 39.37)

shiftPath :: Unit -> (Double, Double) -> Path -> Path
shiftPath u xy (Path layer trace ps) = Path layer trace (map (shiftPoint u xy) ps)

shiftPaths :: [Path] -> Unit -> (Double, Double) -> [Path]
shiftPaths pas u xy = map (shiftPath u xy ) pas



pathtest = Path (L 0) (0, 0) [(Imperial 0, Imperial 0)]

file :: FilePath
file = "src/test.osm"

main :: IO ()
main = do
    paths <-  parseFile file
    let newPaths = shiftPaths paths MM (10, 24)
    writeFile "src/test_out.osm" (showPaths newPaths)

