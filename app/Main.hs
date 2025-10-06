module Main where

import Options.Applicative
import System.Process (callCommand)
import qualified Commands.Arpeggio as Arpeggio
import qualified Commands.Note as Note

data Command
    = Arpeggio Arpeggio.ArpeggioOptions
    | Note Note.NoteOptions

commandParser :: Parser Command
commandParser = subparser
    ( command "arpeggio" (info (Arpeggio <$> Arpeggio.parser <**> helper)
        (progDesc "Generate an arpeggio pattern"))
   <> command "note" (info (Note <$> Note.parser <**> helper)
        (progDesc "Play a single note")) )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "Generate music with Euterpea"
   <> header "simple-euterpea - a music generation CLI" )

main :: IO ()
main = do
    cmd <- execParser opts
    case cmd of
        Arpeggio opts -> do
            Arpeggio.run opts
            callCommand $ "timidity " ++ Arpeggio.output opts
        Note opts -> do
            Note.run opts
            callCommand $ "timidity " ++ Note.output opts
