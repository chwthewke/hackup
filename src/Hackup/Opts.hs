module Hackup.Opts (Opts, cmdOpts, configFile, dryRun) where

import           Control.Monad
import           Options.Applicative
import           System.Environment

import           Hackup.Types

data Opts = Opts { configFile :: String
                 , dryRun :: Bool
--                 , quiet :: Bool
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
       CompletionInvoked _ -> error "unexpected completion invoked"

cmdOpts :: TryIO Opts
cmdOpts = ErrorT $ getArgs >>= parseCommandLine
