{-# LANGUAGE OverloadedStrings     #-}
module Parser.GlobalParser (parseLabel) where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString.Char8 as C

import Type.DataType

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


