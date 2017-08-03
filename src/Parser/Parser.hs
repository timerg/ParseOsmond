{-# LANGUAGE OverloadedStrings     #-}
module Parser.Parser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString.Char8 as C

import Parser.ParsePath
import Parser.ParsePreset
import Type.DataType




parsePaths :: Parser [Path]
parsePaths  = do
    result <- sepBy1 parsePath (C8.char '\n')
    return result

parseFile :: FilePath -> IO ([Path])
parseFile f = do
    input <- readFile f
    -- print $ showPaths <$> (parseOnly parsePaths (C.pack input))
    case parseOnly parsePaths (C.pack input) of
        Right ps -> return ps
        Left s -> error s


parseFileTest :: FilePath -> IO ()
parseFileTest f = do
    input <- readFile f
    parseTest parsePaths (C.pack input)