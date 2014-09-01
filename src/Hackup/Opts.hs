module Hackup.Opts (Opts, cmdOpts, configFile, dryRun, dumpConfig) where

import           Control.Error
import           Control.Monad
import           Options.Applicative
import           System.Environment


data Opts = Opts { configFile :: String
                 , dryRun     :: Bool
                 , dumpConfig :: Bool
                 } deriving (Show, Eq)

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
  <*> switch (
    long "dump-config" 
    <> help "dump the internal representation of the config file.")

parserInfo :: ParserInfo Opts
parserInfo = info (helper <*> parser) (
  fullDesc
    <> progDesc "Create configurable backups."
    <> header "hackup 0.1")

parseCommandLine :: [String] -> IO (Either String Opts)
parseCommandLine args =
  case execParserPure (prefs idm) parserInfo args
    of Success opts        -> return $ Right opts
       Failure f           -> liftM (Left . fst . execFailure f) getProgName
       CompletionInvoked _ -> return $ Left "unexpected completion invoked"

cmdOpts :: EitherT String IO Opts
cmdOpts = EitherT $ getArgs >>= parseCommandLine
