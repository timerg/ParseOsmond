{-# LANGUAGE OverloadedStrings     #-}
module ParsePath (parsePath) where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Prelude hiding(takeWhile)
import Data.Word (Word8)

import DataType


getLayer :: Char -> Int -> Layer
getLayer 'L' i = L i
getLayer 'S' i = S i
getLayer 'M' i = M i
getLayer 'A' i = A i
getLayer c i = error "Undefined Layer identifier"

space' :: Parser String
space' = do
    x <- C8.space
    return " "

string_m :: Parser String
string_m = do
    C8.char 'm'
    C8.space
    return ("m ")


parseLabel :: Parser Label
parseLabel = do
    l <- C8.double
    unit <- C8.choice [space', string_m]
    if unit /= "m "
        then return (Imperial l)
        else return (Metric l)

parsePoint :: Parser Point
parsePoint = do
    xl <- parseLabel
    yl <- parseLabel
    return (xl, yl)

parseTrace :: Parser Trace
parseTrace = do
    C8.char 'W'
    C8.skipSpace
    w <- C8.double
    C8.skipSpace
    C8.char 'S'
    C8.skipSpace
    s <- C8.double
    return (w, s)



parsePath :: Parser Path
parsePath = do
    string "Path"
    C8.skipSpace
    layerName <- C8.anyChar
    C8.skipSpace
    layerPosition <- C8.decimal
    let layer = getLayer layerName layerPosition
    C8.skipSpace
    C8.char '{'
    C8.skipSpace
    startpoint <- parsePoint
    trace <- parseTrace
    C8.skipSpace
    points <- manyTill parsePoint (C8.char '}')
    return $ Path layer trace (startpoint:points)




parseFileTest :: IO ()
parseFileTest = do
    input <- readFile "src/test.osm"
    print $ parseOnly parsePath (C.pack input)
