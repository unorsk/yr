module Main where

import Control.Exception (try)
import Data.Maybe (catMaybes)
import Network.HTTP.Conduit (HttpException)
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatusCode
  , httpJSONEither
  , parseRequest_
  )
import Options.Applicative

import WeatherModel
  ( DayInterval (..)
  , Forecast (..)
  , ShortInterval (..)
  , Temperature (..)
  , WeatherSymbol (..)
  )

data Extended = Normal | Extended deriving (Eq, Show)

data CommandLineArgs = CommandLineArgs
  { city :: String
  , extended :: Extended
  }

-- Looking up unicode symbols here:
-- https://www.cogsci.ed.ac.uk/~richard/utf-8.cgi?input=%F4%80%87%80&mode=char
main :: IO ()
main =
  doForecast
    =<< execParser
      ( info
          (commandLineArgs <**> helper)
          (fullDesc <> progDesc "Print weather forecast for CITY")
      )

doForecast :: CommandLineArgs -> IO ()
doForecast arg =
  let forecastPrintingF = case extended arg of
        Normal -> printForecast
        Extended -> printForecastExetended
   in do
        forecast <- fetchForecast arg.city
        case forecast of
          Left errorMessage -> putStrLn errorMessage
          Right f -> putStrLn $ forecastPrintingF f

pad :: Int -> Int -> a -> [a]
pad used total c =
  replicate (total - used) c

printWeatherSymbols :: [WeatherSymbol] -> String
printWeatherSymbols clouds =
  let cloudsLine = concatMap (\c -> pad (length $ show c) 3 ' ' <> show c) clouds
   in let padding = pad (length cloudsLine) (4 * 3) ' '
       in padding <> cloudsLine

printForecast :: Forecast -> String
printForecast forecast =
  let interval = take 1 forecast.shortIntervals
   in concatMap
        ( \i ->
            show (i.temperature.value)
              <> "Сº  "
              <> show i.precipitation
              <> "  "
              <> show i.symbolCode
        )
        interval

printForecastExetended :: Forecast -> String
printForecastExetended forecast =
  let dayLines =
        map
          ( (\(t, l) -> show t <> "  " <> printWeatherSymbols l <> " \r\n")
              . (\i -> (i.temperature, catMaybes i.sixHourSymbols))
          )
          (forecast.dayIntervals)
   in concat dayLines

-- https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#introduction
commandLineArgs :: Parser CommandLineArgs
commandLineArgs =
  CommandLineArgs
    <$> strOption
      ( long "city"
          <> help "City ID"
          <> showDefault
          <> Options.Applicative.value "1-72837" -- Oslo, ofc
          <> metavar "CITY"
      )
    <*> flag
      Normal
      Extended
      ( long "extended"
          <> short 'e'
          <> help "Extended forecast"
          <> showDefault
      )

fetchForecast :: String -> IO (Either String Forecast)
fetchForecast city = do
  res <-
    try $
      httpJSONEither
        (parseRequest_ ("https://www.yr.no/api/v0/locations/" <> city <> "/forecast"))
  pure $ case res of
    -- TODO BETTER ERROR HANDLING
    Left (e :: HttpException) -> Left $ show e
    Right response -> case getResponseBody response of
      -- TODO BETTER ERROR HANDLING
      Left _e ->
        Left $
          "Couldn't parse response. Response code: "
            <> show (getResponseStatusCode response)
            <> show _e
      Right forecast -> Right forecast
