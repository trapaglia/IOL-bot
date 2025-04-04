module Utils
    ( getCurrentTimeArgentina
    ) where

import Data.Time (UTCTime, getCurrentTime, addUTCTime)

-- Obtener el tiempo actual en Argentina (UTC-3)
getCurrentTimeArgentina :: IO UTCTime
getCurrentTimeArgentina = addUTCTime (-10800) <$> getCurrentTime
