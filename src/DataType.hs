module DataType where

-- For Parsing
data Layer = L Int | S Int | M Int | A Int deriving (Show)
type Width = Double
type Spacing = Double
type Trace = (Width, Spacing)
data Label = Imperial Double | Metric Double
type Point = (Label, Label)
data Path = Path Layer Trace [Point]

-- For Operation
data Unit = MM | MIL


pathtest = Path (L 0) (0, 0) [(Imperial 0, Imperial 0)]


-- instance Show Trace where
--     show (Trace w s) = "W " ++ (show w) ++ "S " ++ (show s)

instance Show Label where
    show (Imperial l) = show l
    show (Metric l) = (show l) ++ "m"

instance Show Path where
    show (Path layer (w, s) (p:ps)) = "Path " ++ (show layer) ++ " { " ++ (show' (p:[])) ++
                "W " ++ (show w) ++ " " ++ "S " ++ (show s) ++ " " ++ (show' ps) ++ "}"
                        where show' [] = ""
                              show' ((lx, ly):ls) = (show lx) ++ " " ++ (show ly) ++ " " ++ (show' ls)


showPaths :: [Path] -> String
showPaths [] = ""
showPaths (pa:pas) = (show pa) ++ "\n" ++ (showPaths pas)
