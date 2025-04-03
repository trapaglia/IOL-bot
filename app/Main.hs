{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types (ApiConfig(..))
import Api (getCredentials, callApi, getCotizacion, getEstadoCuenta)
import qualified Data.ByteString.Lazy.Char8 as BL

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
            _ <- getEstadoCuenta config
            -- _ <- getCotizacion config "GGAL"
            return ()
            
        _ -> putStrLn "Error: Credenciales no encontradas en .env"