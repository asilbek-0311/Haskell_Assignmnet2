{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V  -- Add this import for the vector package
import Data.List (groupBy, sortOn)
import Data.Function (on)

data CovidData = CovidData
  { date :: String,
    state :: String,
    deathsNew :: Int
  }
  deriving (Show)

instance FromNamedRecord CovidData where
  parseNamedRecord r = CovidData <$> r .: "date" <*> r .: "state" <*> r .: "deaths_new"

-- Function to read the CSV file and parse it into a list of CovidData records
readCovidData :: FilePath -> IO [CovidData]
readCovidData filePath = do
  content <- BL.readFile filePath
  case decodeByName content of
    Left err -> error err
    Right (_, v) -> return (V.toList v)  -- Convert Vector to list


-- Function to answer question 1: Which states in Malaysia have the highest deaths due to Covid?
highestDeathsByState :: [CovidData] -> [(String, Int)]
highestDeathsByState covidData =
  let groupedByState = groupBy ((==) `on` state) $ sortOn state covidData
      stateDeaths = map (\group -> (state $ head group, sum $ map deathsNew group)) groupedByState
   in sortOn (negate . snd) stateDeaths

-- Function to answer question 2: What is the average death per day for Malaysia in the provided data?
averageDeathsPerDay :: [CovidData] -> Double
averageDeathsPerDay covidData =
  let groupedByDate = groupBy ((==) `on` date) $ sortOn date covidData
      dailyDeaths = map (sum . map deathsNew) groupedByDate
   in fromIntegral (sum dailyDeaths) / fromIntegral (length dailyDeaths)

-- Function to answer question 3: Which of the five states has the lowest death in the provided data?
lowestDeathsByState :: [CovidData] -> [(String, Int)]
lowestDeathsByState covidData =
  let stateDeaths = map (\group -> (state $ head group, sum $ map deathsNew group)) $ groupBy ((==) `on` state) $ sortOn state covidData
  in take 5 $ sortOn snd stateDeaths

main :: IO ()
main = do
  covidData <- readCovidData "deaths_state.csv"

  putStrLn "1. States in Malaysia with the highest deaths due to Covid:"
  mapM_ (\(s, d) -> putStrLn $ s ++ ": " ++ show d) (highestDeathsByState covidData)

  putStrLn "\n2. Average death per day for Malaysia in the provided data:"
  print $ averageDeathsPerDay covidData

  putStrLn "\n3. 5 States with the lowest death in the provided data:"
  mapM_ (\(s, d) -> putStrLn $ s ++ ": " ++ show d) (lowestDeathsByState covidData)
