
# Project Documentation

## Overview

This project, named **CovidDataAnalyse**, is a Haskell program designed to analyze COVID-19 data for states in Malaysia. The program reads data from a CSV file containing information about COVID-19 deaths, processes the data, and answers specific questions related to the dataset.

## Project Structure

The project consists of the following files:

- **Main.hs:** The main Haskell file containing the program's entry point and logic.
- **death_state.csv:** The dataset in CSV format containing COVID-19 death data for different states in Malaysia.

## Dependencies

The project relies on the following Haskell libraries:

- **cassava:** Used for CSV parsing.
- **bytestring:** Used for handling lazy ByteString.
- **vector:** Used for efficient representation of data after parsing.

These dependencies are specified in the `.cabal` file, and the project utilizes the Cabal build system for managing dependencies.

## Data Processing

### Data Type Definition

The program defines a data type called `CovidData` to represent the structure of the COVID-19 data. This data type includes fields for the date, state, and the number of new deaths.

```haskell
data CovidData = CovidData
  { date :: String
  , state :: String
  , deathsNew :: Int
  } deriving (Show)
```

### CSV Parsing

The `cassava` library is used to parse the CSV data into a list of `CovidData` records. The `FromNamedRecord` instance is defined to specify how to parse each record.

```haskell
instance FromNamedRecord CovidData where
  parseNamedRecord r = CovidData <$> r .: "date" <*> r .: "state" <*> r .: "deaths_new"
```

The `readCovidData` function reads the CSV file and returns a list of `CovidData` records.

```haskell
readCovidData :: FilePath -> IO [CovidData]
readCovidData filePath = do
  content <- BL.readFile filePath
  case decodeByName content of
    Left err -> error err
    Right (_, v) -> return (V.toList v)
```

## Data Analysis

The program provides functions for analyzing the COVID-19 data:

1. **`highestDeathsByState` Function:**
   - Groups the data by state.
   - Calculates the total deaths for each state.
   - Returns a list of states with the highest deaths.

2. **`averageDeathsPerDay` Function:**
   - Groups the data by date.
   - Calculates the daily deaths.
   - Computes the average deaths per day.

3. **`lowestDeathsByState` Function:**
   - Groups the data by state.
   - Calculates the total deaths for each state.
   - Returns the state with the lowest total deaths.

## Main Function

The `main` function serves as the entry point of the program. It reads the COVID-19 data, performs data analysis, and prints the results.

```haskell
main :: IO ()
main = do
  covidData <- readCovidData "death_state.csv"

  -- Display the results
  -- ...

```

## Results

The program answers three specific questions about the COVID-19 data:
1. States in Malaysia with the highest deaths due to Covid.
2. Average death per day for Malaysia in the provided data.
3. 5 States with the lowest death in the provided data.

```
1. States in Malaysia with the highest deaths due to Covid:
   
Selangor: 11033
Johor: 4742
Sabah: 3212
W.P. Kuala Lumpur: 2861
Kedah: 2757
Perak: 2170
Pulau Pinang: 2086
Sarawak: 1796
Negeri Sembilan: 1547
Kelantan: 1429
Melaka: 1216
Pahang: 1037
Terengganu: 905
Perlis: 199
W.P. Labuan: 159
W.P. Putrajaya: 30

2. Average death per day for Malaysia in the provided data:
   
28.446059678653405

3. 5 States with the lowest death in the provided data:
   
W.P. Putrajaya: 30
W.P. Labuan: 159
Perlis: 199
Terengganu: 905
Pahang: 1037
```
