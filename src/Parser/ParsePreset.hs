{-# LANGUAGE OverloadedStrings     #-}
module Parser.ParsePreset  where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Prelude hiding(takeWhile)
import Data.Word (Word8)

import Type.DataType
import Parser.GlobalParser


type RGB = (Int, Int, Int)

data View = View {
-- wa: working area
    wadWidth :: Double,
    waHeight :: Double,
    waMargin :: Double,
    viewMag :: Double,
    center :: (Label, Label),
    grid :: (Label, Label),
    index :: (Label, Label),
    printScale :: Double,
    showNames     :: Bool,
    showValues    :: Bool,
    fullWidth     :: Bool,
    outlines      :: Bool,
    showHoles     :: Bool,
    showPinNames  :: Bool,
    showOrigin    :: Bool,
    showSolidPads     :: Bool,
    showVectorText    :: Bool,
    showSelectedSignalTrace :: Bool,
    savedFrameString :: [Int],
    padFillColor :: RGB,
    gridColor :: RGB,
    holeColor :: RGB,
    backgroundColor :: RGB,
    selectedSignalTraceColor :: RGB,
    customGrid :: [(Label, Label)]
} deriving (Show)

(<>) :: Monad m =>  m a -> m b -> m a
(<>) = flip (>>)

parsePoint :: Parser (Label, Label)
parsePoint = do
    xl <- parseLabel
    yl <- parseLabel
    return (xl, yl)

parseRGB :: Parser RGB
parseRGB = do
    r <- C8.decimal
    C8.skipSpace
    g <- C8.decimal
    C8.skipSpace
    b <- C8.decimal
    C8.skipSpace
    return (r, g, b)

parseCustomGrid :: Parser (Label, Label)
parseCustomGrid = do
            string "CustomGrid \""
            xl <- parseLabel'
            yl <- option xl $ (string " x " >> parseLabel')
            string "\""
            C8.skipSpace
            return (xl, yl)
                where parseLabel' = do
                           val <- C8.double
                           C8.skipSpace
                           label <- choice [(Metric val) <$ (string "mm") , (Imperial val) <$ (string "mils")]
                           return label

string' a = C8.skipSpace >> (string a)

-- stringList s = (option False (True <$ (string s))) <> C8.skipSpace
stringList s = do
    x <- option False (True <$ (string s))
    C8.skipSpace
    return x

parseView :: Parser View
parseView = do
    string "View {"
    C8.skipSpace
    string "Width "
    width <- C8.double
    C8.skipSpace
    string "Height "
    height <- C8.double
    C8.skipSpace
    margin <- let parsem = (string "Margin ") >> (C8.double) <> (C8.skipSpace) in option 0.0 parsem
    string "Mag "
    mag <- C8.double
    C8.skipSpace
    string "Center "
    center <- parsePoint
    C8.skipSpace
    string "Grid "
    grid <- parsePoint
    C8.skipSpace
    string "Index "
    index <- parsePoint
    C8.skipSpace
    string "PrintScale "
    printScale <- C8.double
    C8.skipSpace
    theFullWidth <- stringList "FullWidth"
    theOutlines <- stringList "Outlines"
    theShowHoles <- stringList "ShowHoles"
    theShowNames <- stringList "ShowNames"
    theShowValues <- stringList "ShowValues"
    theShowPinNames <- stringList "ShowPinNames"
    theShowOrigin <- stringList "ShowOrigin"
    theSolidPads <- stringList "SolidPads"
    theVectorText <- stringList "VectorText"
    theShowSelectedSignalTrace <- stringList "ShowSelectedSignalTrace"
    -- C8.skipSpace
    string "SavedFrameString \""
    theSavedFrameString <- manyTill (C8.decimal <> C8.skipSpace) (string' "\"" >> C8.skipSpace)
    string "PadFillColor "
    thepadFillColor <- parseRGB
    string "GridColor "
    thegridColor <- parseRGB
    string "HoleColor "
    theholeColor <- parseRGB
    string "BackgroundColor "
    thebackgroundColor <- parseRGB
    string "SelectedSignalTraceColor "
    theSelectedSignalTraceColor <- parseRGB
    theCustomGrid <- manyTill parseCustomGrid (string "}")
    return $ View width
                height
                margin
                mag
                center
                grid
                index
                printScale
                theFullWidth
                theOutlines
                theShowHoles
                theShowNames
                theShowValues
                theShowPinNames
                theShowOrigin
                theSolidPads
                theVectorText
                theShowSelectedSignalTrace
                theSavedFrameString
                thepadFillColor
                thegridColor
                theholeColor
                thebackgroundColor
                theSelectedSignalTraceColor
                theCustomGrid
-- --

parseView :: Parser Setup






-- --