{-# LANGUAGE OverloadedStrings     #-}
module Parser.ParsePreset where

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
    index :: (Int, Int),
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


string' a = C8.skipSpace >> (string a)


stringList s = do
    result <- let toTrue "" = False
                  toTrue s  = True
              in option False $ (toTrue <$> (string s) <> C8.skipSpace)
    return result

parseView :: Parser ()
parseView = do
    string "View {"
    C8.skipSpace
    string "Width "
    width <- C8.double
    C8.skipSpace
    string "Height "
    height <- C8.double
    C8.skipSpace
    margin <- let parsem = (string "Margin ") >> (C8.double) <> (C8.skipSpace) in try parsem
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
    string "SavedFrameString \""
    many' (C8.decimal <> C8.space)
    
    return ()









-- --