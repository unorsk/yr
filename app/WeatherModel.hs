module WeatherModel
  ( WeatherSymbol (..)
  , Precipitation (..)
  , SymbolCode (..)
  , Temperature (..)
  , DayInterval (..)
  , ShortInterval (..)
  , Forecast (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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

data Precipitation = Precipitation
  { value :: Float
  , max :: Maybe Float
  , min :: Maybe Float
  , probability :: Maybe Int
  }
  deriving (Eq, Generic, FromJSON)

instance Show Precipitation where
  show (Precipitation vvalue _max _min probability) =
    ( case probability of
        Nothing -> ""
        Just p -> show p <> "% "
    )
      <> show vvalue
      <> "mm "

data SymbolCode = SymbolCode
  { next1Hour :: Maybe WeatherSymbol
  , next6Hours :: Maybe WeatherSymbol
  , next12Hours :: Maybe WeatherSymbol
  }
  deriving (Eq, Generic, FromJSON)

printMaybeWeather :: Maybe WeatherSymbol -> String
printMaybeWeather = maybe " " show

instance Show SymbolCode where
  show (SymbolCode next1Hour next6Hours next12Hours) =
    printMaybeWeather next1Hour
      <> "  "
      <> printMaybeWeather next6Hours
      <> "  "
      <> printMaybeWeather next12Hours

data Temperature = Temperature
  { value :: Float
  , max :: Maybe Float
  , min :: Maybe Float
  }
  deriving (Eq, Generic, FromJSON)

instance Show Temperature where
  show (Temperature _value mmax mmin) =
    show (fromMaybe 0.0 mmin :: Float)
      <> "Cº "
      <> show (fromMaybe 0.0 mmax)
      <> "Cº "

data DayInterval = DayInterval
  { sixHourSymbols :: [Maybe WeatherSymbol]
  , temperature :: Temperature
  , start :: UTCTime
  , end :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

data ShortInterval = ShortInterval
  { symbolCode :: SymbolCode
  , temperature :: Temperature
  , precipitation :: Precipitation
  , start :: UTCTime
  , end :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

data Forecast = Forecast
  { created :: UTCTime
  , dayIntervals :: [DayInterval]
  , shortIntervals :: [ShortInterval]
  }
  deriving (Eq, Show, Generic, FromJSON)
