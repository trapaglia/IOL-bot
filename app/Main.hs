{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyLib ()

import Network.Socket
import Network.Socket.ByteString (recv, send)
-- import Control.Concurrent.MVar
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Aeson (FromJSON(..), Value(Object), (.:)) --toJSON, 
import qualified Data.ByteString.Char8 as BC
import Control.Monad (mzero)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
-- import qualified Data.ByteString.Lazy as BL
-- import Data.String (fromString)

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
    let host = "api.invertironline.com"
        port = "443"
    
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    let serverAddr = head addrInfo

    -- Create and connect the socket
    clientSocket <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect clientSocket (addrAddress serverAddr)

    -- Define the HTTP POST request
    let postData = BC.pack $ "username=" ++ username ++ "&password=" ++ password ++ "&grant_type=password"
        postRequest = BC.concat
            [ BC.pack "POST /token HTTP/1.1\r\n"
            , BC.pack $ "Host: " ++ host ++ "\r\n"
            , BC.pack "Content-Type: application/x-www-form-urlencoded\r\n"
            , BC.pack $ "Content-Length: " ++ show (BC.length postData) ++ "\r\n"
            , BC.pack "\r\n"
            , postData
            ]

    -- Send the request
    _ <- send clientSocket postRequest

    -- Receive and print the response
    response <- recv clientSocket 4096
    BC.putStrLn response

    -- Close the socket
    close clientSocket

    -- Parse the response to extract the token
    let responseStr = BC.unpack response
        startIndex = BC.length (BC.pack "access_token\":\"") + BC.length (BC.pack responseStr)
        endIndex = length responseStr - 1
        token = BC.unpack (BC.take (endIndex - startIndex) (BC.drop startIndex response))

    return (Just token)

-- Función principal para probar la autenticación
main :: IO ()
main = do

    -- Set up buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- -- Create MVar for state management
    -- shutdownVar <- newMVar True

    (mUser, mPass) <- getCredentials
    case (mUser, mPass) of
        (Just user, Just pass) -> do
            token <- authenticate user pass
            case token of
                Just t  -> putStrLn $ "Token obtenido: " ++ t
                Nothing -> putStrLn "Error al obtener el token"
        _ -> putStrLn "Error: No se encontraron credenciales en el archivo .env"
