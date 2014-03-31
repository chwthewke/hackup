module Hackup.Opts (Opts, cmdOpts, configFile, dryRun) where

import Hackup.Types
import Options.Applicative
import Options.Applicative.Extra
import System.Environment

data Opts = Opts { configFile :: String
                 , dryRun :: Bool
--                 , quiet :: Bool
                 } deriving (Show, Eq)

programName :: String
programName = "hackup"

parser :: Parser Opts
parser = Opts
  <$> strOption (
    long "config-file"
      <> short 'f'
      <> metavar "file"
      <> help "required, the configuration file to read.")
  <*> switch (
    long "dry-run"
      <> short 'n'
      <> help "don't run the backup, just show what would happen.")

parserInfo :: ParserInfo Opts
parserInfo = info (helper <*> parser) (
  fullDesc
    <> progDesc "Create configurable backups."
    <> header "hackup 0.1")

co :: IO (Maybe Opts)
co = getArgs >>= return . execParserMaybe parserInfo

cmdOpts :: TryT IO Opts
cmdOpts = undefined

