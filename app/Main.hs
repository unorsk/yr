{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)

data WeatherSymbol where
  WeatherSymbol :: String -> WeatherSymbol
  deriving (Eq, Generic, FromJSON)

instance Show WeatherSymbol where
  show (WeatherSymbol []) = "idk"
  show (WeatherSymbol "rainandthunder") = "\x1001DE"
  show (WeatherSymbol "fair_day") = "\x1001AD"
  show (WeatherSymbol "rain") = "\x1001C6"
  show (WeatherSymbol "partlycloudy_day") = "\x1001D4"
  show (WeatherSymbol "partlycloudy_night") = "\x1001DA"
  show (WeatherSymbol "cloudy") = "\x1001C2"
  show (WeatherSymbol "clearsky_day") = "\x1001AD"
  show (WeatherSymbol "fair_night") = "\x1001B9"
  show (WeatherSymbol "rainshowers_day") = "\x1001C8"
  show (WeatherSymbol "rainshowers_night") = "\x1001DC"
  show (WeatherSymbol "heavyrain") = "\x1001C8"
  show (WeatherSymbol "lightrain") = "\x1001C4"
  show (WeatherSymbol "lightrainshowersandthunder_day") = "\x1001D8"
  show (WeatherSymbol r) = r

data DayInterval = DayInterval
  {
    sixHourSymbols :: [Maybe WeatherSymbol]
    , start :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

data Forecast = Forecast 
  {
    created :: UTCTime
    , dayIntervals :: [DayInterval]
  }
  deriving (Eq, Show, Generic, FromJSON)

-- https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#introduction
main :: IO ()
main = do
  forecast <- fetchForecast
  case forecast of
    Left errorMessage -> putStrLn errorMessage
    Right f ->
      let dayLines = map (\i -> catMaybes i.sixHourSymbols) f.dayIntervals in
        traverse_ (\(line::[WeatherSymbol]) -> do
          let strLine = map (\l -> " " <> show l <> " ") line in
            putStrLn $ concat strLine
          ) dayLines

fetchForecast :: IO (Either String Forecast)
fetchForecast = do
  res <- httpJSON "https://www.yr.no/api/v0/locations/1-72837/forecast"
  let r :: Forecast = getResponseBody res in
    pure (Right r)
