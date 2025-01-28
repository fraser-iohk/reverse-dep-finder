module Options where

import Options.Applicative

data FinderOptions = FinderOptions
  { hieDirectory :: FilePath
  , cabalPlanFile :: Maybe FilePath
  } deriving (Show, Eq, Ord)

getFinderOptions :: IO FinderOptions
getFinderOptions =
  execParser $ info (finderOptionsParser <**> helper) $ mconcat
    [ fullDesc
    ]

finderOptionsParser :: Parser FinderOptions
finderOptionsParser =
  FinderOptions <$> hieDirectoryParser
                <*> cabalPlanFileParser

hieDirectoryParser :: Parser FilePath
hieDirectoryParser = do
  option str $ mconcat
    [ long "hies"
    , short 'h'
    , metavar "HIES_DIRECTORY"
    ]

cabalPlanFileParser :: Parser (Maybe FilePath)
cabalPlanFileParser = do
  optional $ option str $ mconcat
    [ long "plan"
    , short 'p'
    , metavar "CABAL_PLAN_FILE"
    ]
