{-# LANGUAGE OverloadedStrings #-}

module MyLib 
    ( getCredentials
    , authenticate
    , AuthResponse(..)
    ) where

import System.Environment (lookupEnv)
import qualified Configuration.Dotenv as Dotenv
import Data.Aeson (FromJSON(..), Value(Object), (.:))
import qualified Data.ByteString.Char8 as BC
import Control.Monad (mzero)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try)

-- Tipo de dato para almacenar el token
data AuthResponse = AuthResponse { accessToken :: String }

instance FromJSON AuthResponse where
    parseJSON (Object v) = AuthResponse <$> v .: "access_token"
    parseJSON _ = mzero

-- Función para obtener las credenciales desde el archivo .env
getCredentials :: IO (Maybe String, Maybe String)
getCredentials = do
    let envPath = "c:\\Users\\Administrador\\Documents\\proyectos\\IOL bot\\.env"
    let config = Dotenv.defaultConfig { Dotenv.configPath = [envPath] }
    _ <- Dotenv.loadFile config
    username <- lookupEnv "IOL_USERNAME"
    password <- lookupEnv "IOL_PASSWORD"
    return (username, password)

-- Función para realizar la autenticación y obtener el token
authenticate :: String -> String -> IO (Maybe String)
authenticate username password = do
    -- Crear un manager TLS que maneja automáticamente los certificados
    manager <- newTlsManager
    
    -- Preparar la request
    initialRequest <- parseRequest "https://api.invertironline.com/token"
    let postData = BC.pack $ "username=" ++ username ++ "&password=" ++ password ++ "&grant_type=password"
        request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyBS postData
            , requestHeaders = 
                [ ("Content-Type", "application/x-www-form-urlencoded")
                ]
            }
    
    -- Enviar la request y manejar la respuesta
    result <- try $ httpLbs request manager
    case result of
        Left e -> do
            putStrLn $ "Error en la petición HTTP: " ++ show (e :: HttpException)
            return Nothing
        Right response -> do
            let status = statusCode $ responseStatus response
            if status == 200
                then do
                    let body = responseBody response
                    putStrLn $ "Respuesta recibida: " ++ show body
                    -- Extraer el token de la respuesta
                    let startIndex = BC.length (BC.pack "access_token\":\"")
                        responseStr = BL.toStrict body
                        endIndex = length (BC.unpack responseStr) - 2
                        token = BC.unpack $ BC.take (endIndex - startIndex) $ BC.drop startIndex responseStr
                    return $ Just token
                else do
                    putStrLn $ "Error: Status code " ++ show status
                    return Nothing
