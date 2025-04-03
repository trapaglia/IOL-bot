{-# LANGUAGE OverloadedStrings #-}

module Api
    ( getCredentials
    , authenticate
    , callApi
    , getCotizacion
    , getEstadoCuenta
    ) where

import System.Environment (lookupEnv)
import qualified Configuration.Dotenv as Dotenv
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Types

-- Variable global para almacenar el token
{-# NOINLINE globalToken #-}
globalToken :: IORef (Maybe String)
globalToken = unsafePerformIO $ newIORef Nothing

-- Función para obtener las credenciales desde el archivo .env
getCredentials :: IO (Maybe String, Maybe String)
getCredentials = do
    let envPath = "c:\\Users\\Administrador\\Documents\\proyectos\\IOL bot\\.env"
    let config = Dotenv.defaultConfig { Dotenv.configPath = [envPath] }
    _ <- Dotenv.loadFile config
    user<- lookupEnv "IOL_USERNAME"
    pass<- lookupEnv "IOL_PASSWORD"
    return (user, pass)

-- Función para extraer el token de la respuesta
extractToken :: BL.ByteString -> Maybe String
extractToken body = do
    response <- decode body :: Maybe AuthResponse
    return $ accessToken response

-- Función para realizar la autenticación y obtener el token
authenticate :: String -> String -> IO (Maybe String)
authenticate user pass = do
    -- Crear un manager TLS que maneja automáticamente los certificados
    manager <- newTlsManager
    
    -- Preparar la request
    initialRequest <- parseRequest "https://api.invertironline.com/token"
    let postData = BC.pack $ "username=" ++ user ++ "&password=" ++ pass ++ "&grant_type=password"
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
                    -- putStrLn $ "Respuesta recibida: " ++ show body
                    -- Extraer el token de la respuesta usando la nueva función
                    let mToken = extractToken body
                    case mToken of
                        Just token -> do
                            writeIORef globalToken (Just token)
                            return $ Just token
                        Nothing -> do
                            putStrLn "Error: No se pudo extraer el token de la respuesta"
                            return Nothing
                else do
                    putStrLn $ "Error: Status code " ++ show status
                    return Nothing

-- Función para llamar a cualquier API con manejo automático del token
callApi :: ApiConfig -> String -> IO (Maybe BL.ByteString)
callApi config apiUrl = do
    manager <- newTlsManager
    currentToken <- readIORef globalToken
    
    let makeRequest token = do
            initialRequest <- parseRequest apiUrl
            let request = initialRequest
                    { method = "GET"
                    , requestHeaders = 
                        [ ("Authorization", "Bearer " <> BC.pack token)
                        , ("Content-Type", "application/json")
                        ]
                    }
            
            result <- try $ httpLbs request manager
            case result of
                Left e -> do
                    putStrLn $ "Error en la petición HTTP: " ++ show (e :: HttpException)
                    return Nothing
                Right response -> do
                    putStrLn $ "Todo bien en la petición HTTP: " ++ show (response)
                    let status = statusCode $ responseStatus response
                    if status == 200
                        then do
                            putStrLn "API call successful"
                            return $ Just $ responseBody response
                        else return Nothing

    case currentToken of
        Just token -> do
            response <- makeRequest token
            case response of
                Just body -> return $ Just body
                Nothing -> do
                    putStrLn "Renovando token..."
                    newToken <- authenticate (username config) (password config)
                    case newToken of
                        Just token' -> makeRequest token'
                        Nothing -> do
                            putStrLn "Error al renovar el token"
                            return Nothing
        Nothing -> do
            putStrLn "No hay token, obteniendo uno nuevo..."
            newToken <- authenticate (username config) (password config)
            case newToken of
                Just token -> makeRequest token
                Nothing -> do
                    putStrLn "Error al obtener el token"
                    return Nothing

-- Función para obtener la cotización de un símbolo
getCotizacion :: ApiConfig -> String -> IO (Maybe CotizacionDetalle)
getCotizacion config symbol = do
    response <- callApi config $ "https://api.invertironline.com/api/v2/Cotizaciones/" ++ symbol ++ "/Todos"
    case response of
        Nothing -> do
            putStrLn "Error al llamar a la API"
            return Nothing
        Just body -> do
            putStrLn $ "Respuesta: " ++ show body
            -- return $ decode body
            let cotizacion = decode body :: Maybe CotizacionDetalle
            case cotizacion of
                Just cot -> do
                    putStrLn $ "Último precio: " ++ show (ultimoPrecio cot)
                    case puntas cot of
                        (p:_) -> putStrLn $ "Spread: " ++ show (precioVenta p - precioCompra p)
                        [] -> putStrLn "No hay puntas disponibles"
                    return $ Just cot
                Nothing -> do
                    putStrLn "Error al decodificar la respuesta"
                    return Nothing

-- Función para obtener el estado de cuenta
getEstadoCuenta :: ApiConfig -> IO (Maybe EstadoCuenta)
getEstadoCuenta config = do
    response <- callApi config "https://api.invertironline.com/api/v2/estadocuenta"
    case response of
        Nothing -> do
            putStrLn "Error al llamar a la API"
            return Nothing
        Just body -> do
            -- putStrLn $ "Respuesta: " ++ show body
            -- return $ decode body
            let estadoCuenta = decode body :: Maybe EstadoCuenta
            case estadoCuenta of
                Just ec -> do
                    putStrLn "Estado de cuenta:"
                    mapM_ (\cuenta -> do
                        putStrLn $ "\nCuenta: " ++ numero cuenta
                        putStrLn $ "Tipo: " ++ tipo cuenta
                        putStrLn $ "Moneda: " ++ moneda cuenta
                        case saldos cuenta of
                            (s:_) -> do
                                putStrLn $ "Saldo: " ++ show (saldo s)
                                putStrLn $ "Comprometido: " ++ show (comprometido s)
                                putStrLn $ "Disponible: " ++ show (disponible s)
                            [] -> putStrLn "No hay información de saldos"
                        ) (cuentas ec)
                    return $ Just ec
                Nothing -> do
                    putStrLn "Error al decodificar la respuesta"
                    return Nothing
