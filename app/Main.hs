{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import MyLib (getCredentials, authenticate)

-- Función principal para probar la autenticación
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    (mUser, mPass) <- getCredentials
    case (mUser, mPass) of
        (Just user, Just pass) -> do
            putStrLn "\nCredentials OK!\n"
            token <- authenticate user pass
            case token of
                Just t  -> putStrLn $ "Token obtenido: " ++ t
                Nothing -> putStrLn "Error al obtener el token"
        _ -> putStrLn "Error: No se encontraron credenciales en el archivo .env"
