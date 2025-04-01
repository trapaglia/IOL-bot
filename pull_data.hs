{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Network.HTTP.Client (RequestBody (..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson (decode, Value(..), (.:))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

-- Tipo de dato para almacenar el token
data AuthResponse = AuthResponse { accessToken :: String }

instance FromJSON AuthResponse where
    parseJSON (Object v) = AuthResponse <$> v .: "access_token"
    parseJSON _ = mzero

-- Función para obtener credenciales desde .env
getCredentials :: IO (Maybe String, Maybe String)
getCredentials = do
    loadFile defaultConfig  -- Carga el archivo .env
    user <- lookupEnv "IOL_USERNAME"
    pass <- lookupEnv "IOL_PASSWORD"
    return (user, pass)

-- Función para realizar la autenticación y obtener el token
authenticate :: String -> String -> IO (Maybe String)
authenticate username password = do
    manager <- newManager tlsManagerSettings
    req <- parseRequest "https://api.invertironline.com/token"
    
    let body = "username=" ++ username ++ "&password=" ++ password ++ "&grant_type=password"
    let req' = req
            { method = "POST"
            , requestBody = RequestBodyBS (B8.pack body)
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            }
    
    response <- httpLBS req'
    let responseBody = getResponseBody response
    
    case decode responseBody of
        Just auth -> return (Just (accessToken auth))
        Nothing   -> return Nothing

-- Función principal para probar la autenticación
main :: IO ()
main = do
    (mUser, mPass) <- getCredentials
    case (mUser, mPass) of
        (Just user, Just pass) -> do
            token <- authenticate user pass
            case token of
                Just t  -> putStrLn $ "Token obtenido: " ++ t
                Nothing -> putStrLn "Error al obtener el token"
        _ -> putStrLn "Error: No se encontraron credenciales en el archivo .env"
