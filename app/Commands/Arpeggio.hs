module Commands.Arpeggio (run, parser, ArpeggioOptions(..)) where

import Euterpea
import Options.Applicative
import Utils

data ArpeggioOptions = ArpeggioOptions
    { notes :: [PitchClass]
    , repetitions :: Int
    , output :: FilePath
    }

parser :: Parser ArpeggioOptions
parser = ArpeggioOptions
    <$> option (maybeReader parsePitchClasses)
        ( long "notes"
       <> short 'n'
       <> metavar "NOTES"
       <> help "Pitch classes (e.g., C,E,G,B)"
       <> value [C, E, G, B]
       <> showDefault )
    <*> option auto
        ( long "reps"
       <> short 'r'
       <> metavar "N"
       <> help "Number of repetitions"
       <> value 4
       <> showDefault )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output MIDI file"
       <> value "output.mid"
       <> showDefault )

parsePitchClasses :: String -> Maybe [PitchClass]
parsePitchClasses s = traverse parsePitchClass (splitOn ',' s)

arpeggiate :: [PitchClass] -> Music Pitch
arpeggiate pitches = line $ map (\p -> note (qn/4) (p, 4)) backDown
    where
        backDown = pitches ++ init (tail (reverse pitches))

arpeggiateNTimes :: Int -> [PitchClass] -> Music Pitch
arpeggiateNTimes n pitches = line $ replicate n (arpeggiate pitches)

run :: ArpeggioOptions -> IO ()
run opts = do
    let music = arpeggiateNTimes (repetitions opts) (notes opts)
    writeMidi (output opts) music
    putStrLn $ "Arpeggio written to " ++ output opts
