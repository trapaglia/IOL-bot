{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( getCurrentTimeArgentina
    ) where

import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

-- Obtener el tiempo actual en Argentina (UTC-3)
getCurrentTimeArgentina :: IO UTCTime
getCurrentTimeArgentina = do
    now <- getCurrentTime
    tz <- getTimeZone now
    let localTime = utcToLocalTime tz now
        argentinaOffset = -180  -- UTC-3 en minutos
        argentinaTime = localTime { zonedTimeZone = minutesToTimeZone argentinaOffset }
    return $ zonedTimeToUTC argentinaTime
