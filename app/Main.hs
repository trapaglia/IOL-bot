{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import MyLib (getCredentials, callApi, ApiConfig(..), getCotizacion)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

-- Función principal para probar la autenticación
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- Obtener credenciales
    (maybeUsername, maybePassword) <- getCredentials
    case (maybeUsername, maybePassword) of
        (Just username, Just password) -> do
            putStrLn "Credentials OK!\n"
            let config = ApiConfig username password
            
            -- Probar obtener cotización
            putStrLn "Obteniendo cotización de GGAL..."
            _ <- getCotizacion config "GGAL"
            return ()
            
        _ -> putStrLn "Error: Credenciales no encontradas en .env"

{-
            response <- callApi config "https://api.invertironline.com/api/v2/estadocuenta"
            case response of
                Just body -> putStrLn $ "Respuesta: " ++ BL.unpack body
                Nothing -> putStrLn "Error al llamar a la API"
-}