{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import MyLib (getCredentials, authenticate, callApi, ApiConfig(..))
import qualified Data.ByteString.Lazy.Char8 as BL

-- Función principal para probar la autenticación
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    (mUser, mPass) <- getCredentials
    case (mUser, mPass) of
        (Just user, Just pass) -> do
            putStrLn "\nCredentials OK!\n"
            -- Crear la configuración de la API
            let config = ApiConfig user pass
            
            -- Ejemplo de llamada a la API
            putStrLn "\nProbando llamada a la API..."
            response <- callApi config "https://api.invertironline.com/api/v2/estadocuenta"
            case response of
                Just body -> putStrLn $ "Respuesta: " ++ BL.unpack body
                Nothing -> putStrLn "Error al llamar a la API"
            
        _ -> putStrLn "Error: No se encontraron credenciales en el archivo .env"
