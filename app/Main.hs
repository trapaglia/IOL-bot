{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import System.Environment (lookupEnv)
import qualified Configuration.Dotenv as Dotenv
import Data.Aeson (FromJSON(..), Value(Object), (.:))
import qualified Data.ByteString.Char8 as BC
import Control.Monad (mzero)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Network.TLS
import Network.TLS.Extra.Cipher
import qualified Data.ByteString.Lazy as BL

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
    result <- Dotenv.loadFile config
    username <- lookupEnv "IOL_USERNAME"
    password <- lookupEnv "IOL_PASSWORD"
    return (username, password)

-- Función para realizar la autenticación y obtener el token
authenticate :: String -> String -> IO (Maybe String)
authenticate username password = do
    let host = "api.invertironline.com"
        port = "443"
    
    -- Set up the socket
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    clientSocket <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect clientSocket (addrAddress serverAddr)

    -- Set up TLS
    let params = (defaultParamsClient host (BC.pack ""))
            { clientSupported = defaultSupported { supportedCiphers = ciphersuite_default }
            , clientShared = (clientShared $ defaultParamsClient host (BC.pack "")) { sharedCAStore = mempty }
            , clientHooks = defaultClientHooks
                { onServerCertificate = \_ _ _ _ -> return []  -- Acepta cualquier certificado
                }
            }

    -- Create TLS context
    ctx <- contextNew clientSocket params
    handshake ctx

    -- Prepare the request
    let postData = BC.pack $ "username=" ++ username ++ "&password=" ++ password ++ "&grant_type=password"
        request = BL.concat
            [ BL.fromStrict $ BC.pack "POST /token HTTP/1.1\r\n"
            , BL.fromStrict $ BC.pack $ "Host: " ++ host ++ "\r\n"
            , BL.fromStrict $ BC.pack "Content-Type: application/x-www-form-urlencoded\r\n"
            , BL.fromStrict $ BC.pack $ "Content-Length: " ++ show (BC.length postData) ++ "\r\n"
            , BL.fromStrict $ BC.pack "\r\n"
            , BL.fromStrict postData
            ]

    -- Send request through TLS
    sendData ctx request

    -- Receive response
    response <- recvData ctx
    putStrLn "Response received:"
    BC.putStrLn response

    -- Close TLS connection and socket
    bye ctx
    contextClose ctx
    close clientSocket

    -- Parse the response
    let startIndex = BC.length (BC.pack "access_token\":\"")
        endIndex = length (BC.unpack response) - 2  -- Adjust for closing quote and brace
        token = BC.unpack $ BC.take (endIndex - startIndex) $ BC.drop startIndex response

    return (Just token)

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
