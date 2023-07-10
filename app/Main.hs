{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import Options.Applicative
import Network.HTTP.Simple (getResponseBody, parseRequest_, getResponseStatusCode, httpJSONEither)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes)
import Control.Exception (try)
import Network.HTTP.Conduit (HttpException)
import Data.Aeson (FromJSON)

newtype CommandLineArgs = CommandLineArgs
  {
    city :: String
  }

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
  show (WeatherSymbol "clearsky_night") = "\x1001C0"
  show (WeatherSymbol "fair_night") = "\x1001B9"
  show (WeatherSymbol "rainshowers_day") = "\x1001C8"
  show (WeatherSymbol "rainshowers_night") = "\x1001DC"
  show (WeatherSymbol "heavyrain") = "\x1001C8"
  show (WeatherSymbol "lightrain") = "\x1001C4"
  show (WeatherSymbol "lightrainshowersandthunder_day") = "\x1001D8"
  show (WeatherSymbol r) = r


data Temperature = Temperature
  {
    value :: Float
    , max :: Float
    , min :: Float
  }
  deriving (Eq, Generic, FromJSON)

instance Show Temperature where
  show (Temperature _value max min) = show min <> " Cº " <> show max <> " Cº "

data DayInterval = DayInterval
  {
    sixHourSymbols :: [Maybe WeatherSymbol]
    , temperature :: Temperature
    , start :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

data Forecast = Forecast
  {
    created :: UTCTime
    , dayIntervals :: [DayInterval]
  }
  deriving (Eq, Show, Generic, FromJSON)

-- Looking up unicode symbols here:
-- https://www.cogsci.ed.ac.uk/~richard/utf-8.cgi?input=%F4%80%87%80&mode=char
main :: IO ()
main = doForecast =<< execParser ( info (commandLineArgs <**> helper) ( fullDesc <> progDesc "Print weather forecast for CITY"))

doForecast :: CommandLineArgs -> IO ()
doForecast arg = do
  forecast <- fetchForecast arg.city
  case forecast of
    Left errorMessage -> putStrLn errorMessage
    Right f -> putStrLn $ printForecast f

pad :: Int -> Int -> a -> [a]
pad used total c =
  replicate (total - used) c

--replicate (total - used) c
printWeatherSymbols :: [WeatherSymbol] -> String
printWeatherSymbols clouds =
  let cloudsLine = concatMap (\c -> pad (length $ show c) 3 ' ' <> show c) clouds in
  let padding = pad (length cloudsLine) (4*3) ' ' in
    padding <> cloudsLine

printForecast :: Forecast -> String
printForecast forecast =
  let dayLines = map ((\(t, l) -> show t <> "  " <> printWeatherSymbols l <> " \r\n") . (\i -> (i.temperature, catMaybes i.sixHourSymbols))) (forecast.dayIntervals) in
    concat dayLines

-- https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#introduction
commandLineArgs :: Parser CommandLineArgs
commandLineArgs = CommandLineArgs
              <$> strOption
                  ( long "city"
                  <> help "City ID"
                  <> showDefault
                  <> Options.Applicative.value "1-72837" -- Oslo, ofc
                  <> metavar "CITY" )

fetchForecast :: String -> IO (Either String Forecast)
fetchForecast city = do
  res <- try $ httpJSONEither (parseRequest_ ("https://www.yr.no/api/v0/locations/" <> city  <> "/forecast"))
  pure $ case res of
    -- TODO BETTER ERROR HANDLING
    Left (e :: HttpException) -> Left $ show e
    Right response -> case getResponseBody response of
      -- TODO BETTER ERROR HANDLING
      Left _e -> Left $ "Couldn't parse response. Response code: " <> show (getResponseStatusCode response)
      Right forecast -> Right forecast