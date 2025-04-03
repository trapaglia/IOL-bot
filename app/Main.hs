{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types (ApiConfig(..), EstadoCuenta(..))
import Api (getCredentials, getCotizacion, getEstadoCuenta)
import Database
import qualified Data.Text as T
import Database.SQLite.Simple (close)
import Control.Monad (forM_)

-- Lista de símbolos para obtener cotizaciones
symbols :: [T.Text]
symbols = ["GGAL", "PAMP", "YPF", "BBAR"]

-- Función principal para probar la autenticación
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- Inicializar base de datos
    putStrLn "Inicializando base de datos..."
    conn <- initializeDatabase

    -- Obtener credenciales
    (maybeUsername, maybePassword) <- getCredentials
    case (maybeUsername, maybePassword) of
        (Just user, Just pass) -> do
            putStrLn "Credentials OK!\n"
            let config = ApiConfig user pass
            
            -- Obtener y guardar estado de cuentas
            putStrLn "\nObteniendo estado de cuenta..."
            maybeEstadoCuenta <- getEstadoCuenta config
            case maybeEstadoCuenta of
                Just ec -> do
                    putStrLn "Guardando estado de cuenta en la base de datos..."
                    forM_ (cuentas ec) $ \cuenta -> do
                        insertEstadoCuenta conn cuenta
                Nothing -> putStrLn "Error al obtener estado de cuenta"

            -- Obtener y guardar cotizaciones
            putStrLn "\nObteniendo cotizaciones..."
            forM_ symbols $ \symbol -> do
                putStrLn $ "Obteniendo cotización para " ++ T.unpack symbol ++ "..."
                maybeCotizacion <- getCotizacion config (T.unpack symbol)
                case maybeCotizacion of
                    Just cotizacion -> do
                        putStrLn $ "Guardando cotización de " ++ T.unpack symbol ++ " en la base de datos..."
                        insertCotizacion conn symbol cotizacion
                    Nothing -> putStrLn $ "Error al obtener cotización para " ++ T.unpack symbol

            -- Verificar datos guardados
            putStrLn "\nVerificando datos guardados..."
            
            putStrLn "\nÚltimo estado de cuenta:"
            latestCuentas <- getLatestEstadoCuenta conn
            forM_ latestCuentas $ \cuenta -> do
                putStrLn $ "Cuenta: " ++ T.unpack (cuentaNumero cuenta)
                putStrLn $ "Moneda: " ++ show (cuentaMoneda cuenta)
                putStrLn $ "Saldo disponible: " ++ show (cuentaDisponible cuenta)

            -- putStrLn "\nÚltimas cotizaciones:"
            -- forM_ symbols $ \symbol -> do
            --     maybeCotizacion <- getLatestCotizacion conn symbol
            --     case maybeCotizacion of
            --         Just cotizacion -> do
            --             putStrLn $ T.unpack symbol ++ ":"
            --             putStrLn $ "  Último precio: " ++ show (cotizacionUltimoPrecio cotizacion)
            --             putStrLn $ "  Spread: " ++ show (cotizacionPrecioVenta cotizacion - cotizacionPrecioCompra cotizacion)
            --         Nothing -> putStrLn $ "No hay datos para " ++ T.unpack symbol

            -- Cerrar la conexión
            close conn
            
        _ -> putStrLn "Error: Credenciales no encontradas en .env"