{-# LANGUAGE OverloadedStrings #-}

module Api
    ( getCredentials
    , authenticate
    , callApi
    , callApiWithMethod
    , getCotizacion
    , getEstadoCuenta
    , updateTicketMarketData
    , enviarOrdenCompra
    , enviarOrdenVenta
    , getDolarMEP
    , compareMEP
    , getPortfolio
    , getCantidadPortfolio
    , OrdenRequest(..)
    ) where

import System.Environment (lookupEnv)
import qualified Configuration.Dotenv as Dotenv
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try)
import Data.IORef
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import Types
import Utils (getCurrentTimeArgentina)
import Data.List (find)

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
                        mToken = extractToken body
                    case mToken of
                        Just token -> do
                            putStrLn $ "\nToken obtenido: " ++ token
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
callApi config apiUrl = callApiWithMethod config "GET" apiUrl Nothing

-- Función genérica para llamar a la API con cualquier método HTTP
callApiWithMethod :: ApiConfig -> String -> String -> Maybe BL.ByteString -> IO (Maybe BL.ByteString)
callApiWithMethod config httpMethod apiUrl body = do
    manager <- newTlsManager
    currentToken <- readIORef globalToken
    
    let makeRequest token = do
            initialRequest <- parseRequest apiUrl
            let request = initialRequest
                    { method = BC.pack httpMethod
                    , requestHeaders = 
                        [ ("Authorization", "Bearer " <> BC.pack token)
                        , ("Content-Type", "application/json")
                        ]
                    , requestBody = maybe (RequestBodyBS "") RequestBodyLBS body
                    }
            
            result <- try $ httpLbs request manager
            case result of
                Left e -> do
                    putStrLn $ "Error en la petición HTTP: " ++ show (e :: HttpException)
                    return Nothing
                Right response -> do
                    let status = statusCode $ responseStatus response
                    if status == 200 || status == 201
                        then do
                            putStrLn "API call successful"
                            return $ Just $ responseBody response
                        else do
                            putStrLn $ "Error: Status code " ++ show status
                            return Nothing

    case currentToken of
        Just token -> do
            response <- makeRequest token
            case response of
                Just body' -> return $ Just body'
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
    response <- callApi config $ "https://api.invertironline.com/api/v2/bCBA/Titulos/" ++ symbol ++ "/CotizacionDetalle"
    case response of
        Nothing -> do
            putStrLn "Error al llamar a la API"
            return Nothing
        Just body -> do
            -- putStrLn $ "Respuesta: " ++ show body
            -- return $ decode body
            let cotizacion = decode body :: Maybe CotizacionDetalle
            case cotizacion of
                Just cot -> do
                    -- putStrLn $ "Cotización: " ++ show cot
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

-- Actualizar ticket con información del mercado
updateTicketMarketData :: ApiConfig -> Ticket -> IO (Maybe Ticket)
updateTicketMarketData config ticket = do
    maybeCotizacion <- getCotizacion config (ticketName ticket)
    case maybeCotizacion of
        Just cotizacion -> do
            now <- getCurrentTimeArgentina
            case puntas cotizacion of
                (p:_) -> return $ Just $ ticket 
                    { puntaCompra = precioCompra p
                    , puntaVenta = precioVenta p
                    , lastUpdate = now
                    }
                [] -> return Nothing
        Nothing -> return Nothing

-- Función para enviar una orden de compra
enviarOrdenCompra :: ApiConfig -> OrdenRequest -> IO Bool
enviarOrdenCompra config orden = do
    let url = "https://api.invertironline.com/api/v2/operar/Comprar"
    response <- callApiWithMethod config "POST" url (Just $ encode orden)
    putStrLn $ "  [ + ! + ] Comprando " ++ show (ordenCantidad orden) ++ " de " ++ ordenSimbolo orden ++ " a " ++ show (ordenPrecio orden)
    return $ isJust response

-- Función para enviar una orden de venta
enviarOrdenVenta :: ApiConfig -> OrdenRequest -> IO Bool
enviarOrdenVenta config orden = do
    let url = "https://api.invertironline.com/api/v2/operar/Vender"
    response <- callApiWithMethod config "POST" url (Just $ encode orden)
    return $ isJust response

-- Función para obtener la cotización del dólar MEP
getDolarMEP :: ApiConfig -> String -> IO (Maybe DolarMEP)
getDolarMEP config symbol = do
    response <- callApi config $ "https://api.invertironline.com/api/v2/Cotizaciones/MEP/" ++ symbol
    case response of
        Nothing -> do
            putStrLn $ "Error al obtener cotización del dólar MEP simbolo " ++ symbol
            return Nothing
        Just body -> do
            let dolarMEP = decode body :: Maybe DolarMEP
            case dolarMEP of
                Just mep -> do
                    putStrLn $ "Dólar MEP simbolo " ++ symbol ++ ": " ++ show mep
                    return $ Just mep
                Nothing -> do
                    putStrLn $ "Error al decodificar la respuesta del dólar MEP simbolo " ++ symbol
                    return Nothing

-- Función para comparar el dólar MEP con el AL30
compareMEP :: ApiConfig -> String -> IO (Double, Double)
compareMEP config symbol = do
    symbolDolarMEP <- getDolarMEP config symbol
    standardDolarMEP <- getDolarMEP config "AL30"
    case (symbolDolarMEP, standardDolarMEP) of
        (Just (DolarMEP symbolMEP), Just (DolarMEP standardMEP)) -> return (symbolMEP, standardMEP)
        (Nothing, Just (DolarMEP standardMEP)) -> return(0.0, standardMEP)
        _ -> return (0.0, 0.0)

-- Función para obtener el portafolio
getPortfolio :: ApiConfig -> IO (Maybe PortfolioResponse)
getPortfolio config = do
    response <- callApi config "https://api.invertironline.com/api/v2/portafolio/argentina"
    case response of
        Nothing -> do
            putStrLn "Error al obtener el portafolio"
            return Nothing
        Just body -> do
            let portfolio = decode body :: Maybe PortfolioResponse
            case portfolio of
                Just p -> return $ Just p
                Nothing -> do
                    putStrLn "Error al decodificar el portafolio"
                    return Nothing

-- Función para obtener la cantidad de un símbolo específico del portafolio
getCantidadPortfolio :: ApiConfig -> String -> IO (Maybe Int)
getCantidadPortfolio config symbol = do
    maybePortfolio <- getPortfolio config
    case maybePortfolio of
        Nothing -> return Nothing
        Just portfolio -> do
            let activo = find (\asset -> tituloSimbolo (assetTitulo asset) == symbol) (portfolioActivos portfolio)
            case activo of
                Nothing -> do
                    putStrLn $ "No se encontró el símbolo " ++ symbol ++ " en el portafolio"
                    return Nothing
                Just asset -> do
                    putStrLn $ "Cantidad de " ++ symbol ++ ": " ++ show (assetCantidad asset)
                    return $ Just (round $ assetCantidad asset)