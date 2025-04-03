{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( getCurrentTimeArgentina
    ) where

import Data.Time (UTCTime, getCurrentTime, minutesToTimeZone, utcToZonedTime, zonedTimeToUTC)

-- Obtener el tiempo actual en Argentina (UTC-3)
getCurrentTimeArgentina :: IO UTCTime
getCurrentTimeArgentina = do
    now <- getCurrentTime
    let argentinaZone = minutesToTimeZone (-180)  -- UTC-3 en minutos
        argentinaTime = utcToZonedTime argentinaZone now
    return $ zonedTimeToUTC argentinaTime
