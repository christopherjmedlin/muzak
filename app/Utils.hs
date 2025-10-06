module Utils where

import Euterpea
import Options.Applicative

parsePitchClass :: String -> Maybe PitchClass
parsePitchClass "C" = Just C
parsePitchClass "Cs" = Just Cs
parsePitchClass "D" = Just D
parsePitchClass "Ds" = Just Ds
parsePitchClass "E" = Just E
parsePitchClass "F" = Just F
parsePitchClass "Fs" = Just Fs
parsePitchClass "G" = Just G
parsePitchClass "Gs" = Just Gs
parsePitchClass "A" = Just A
parsePitchClass "As" = Just As
parsePitchClass "B" = Just B
parsePitchClass _ = Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str =
    let (before, after) = break (== delim) str
    in before : case after of
        [] -> []
        _:rest -> splitOn delim rest
