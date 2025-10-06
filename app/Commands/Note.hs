module Commands.Note (run, parser, NoteOptions(..)) where

import Euterpea
import Options.Applicative
import Utils

data NoteOptions = NoteOptions
    { pitch :: PitchClass
    , octave :: Octave
    , duration :: Rational
    , output :: FilePath
    }

parser :: Parser NoteOptions
parser = NoteOptions
    <$> option (maybeReader parsePitchClass)
        ( long "pitch"
       <> short 'p'
       <> metavar "PITCH"
       <> help "Pitch class (C, D, E, F, G, A, B, or sharps with 's': Cs, Ds, etc.)"
       <> value C
       <> showDefault )
    <*> option auto
        ( long "octave"
       <> short 'o'
       <> metavar "OCTAVE"
       <> help "Octave number (0-8)"
       <> value 4
       <> showDefault )
    <*> option auto
        ( long "duration"
       <> short 'd'
       <> metavar "DURATION"
       <> help "Duration (0.25=sixteenth, 0.5=eighth, 1=quarter, 2=half, 4=whole)"
       <> value 1
       <> showDefault )
    <*> strOption
        ( long "output"
       <> metavar "FILE"
       <> help "Output MIDI file"
       <> value "output.mid"
       <> showDefault )

run :: NoteOptions -> IO ()
run opts = do
    let music = note (duration opts) (pitch opts, octave opts)
    writeMidi (output opts) music
    putStrLn $ "Note written to " ++ output opts
