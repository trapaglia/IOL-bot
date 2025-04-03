{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types (ApiConfig(..), EstadoCuenta(..), Estado(..), Precios(..), Ticket(..))
import Api (getCredentials, getCotizacion, getEstadoCuenta)
import Database
import qualified Data.Text as T
import Database.SQLite.Simple (close)
import Control.Monad (forM_)

-- Lista de símbolos para obtener cotizaciones
symbols :: [T.Text]
symbols = ["GGAL"] --"PAMP", "YPF", "BBAR"]

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
            
            -- -- Obtener y guardar estado de cuentas
            -- putStrLn "\nObteniendo estado de cuenta..."
            -- maybeEstadoCuenta <- getEstadoCuenta config
            -- case maybeEstadoCuenta of
            --     Just ec -> do
            --         putStrLn "Guardando estado de cuenta en la base de datos..."
            --         forM_ (cuentas ec) $ \cuenta -> do
            --             insertEstadoCuenta conn cuenta
            --     Nothing -> putStrLn "Error al obtener estado de cuenta"

            -- Obtener y guardar cotizaciones
            putStrLn "\nObteniendo cotizaciones..."
            forM_ symbols $ \symbol -> do
                putStrLn $ "Obteniendo cotización para " ++ T.unpack symbol ++ "..."
                maybeCotizacion <- getCotizacion config (T.unpack symbol)
                case maybeCotizacion of
                    Just cotizacion -> do
                        putStrLn $ "Guardando cotización de " ++ T.unpack symbol ++ " en la base de datos..."
                        -- insertCotizacion conn symbol cotizacion
                    Nothing -> putStrLn $ "Error al obtener cotización para " ++ T.unpack symbol

            -- Probar funcionalidad de tickets
            putStrLn "\nProbando funcionalidad de tickets..."
            let testTicket = Ticket
                    { ticketName = "GGAL"
                    , estado = Waiting
                    , precios = Precios
                        { compra1 = 1000.0
                        , compra2 = 950.0
                        , venta1 = 1100.0
                        , venta2 = 1200.0
                        , takeProfit = 1300.0
                        , stopLoss = 900.0
                        }
                    , puntaCompra = 1050.0
                    , puntaVenta = 1080.0
                    , lastUpdate = read "2025-04-03 04:12:57 UTC"
                    }

            putStrLn "Insertando ticket de prueba..."
            insertTicket conn testTicket

            putStrLn "\nObteniendo ticket específico..."
            maybeTicket <- getTicket conn "GGAL"
            case maybeTicket of
                Just ticket -> do
                    putStrLn $ "Ticket encontrado: " ++ ticketName ticket
                    putStrLn $ "Estado: " ++ show (estado ticket)
                    putStrLn $ "Precio compra 1: " ++ show (compra1 $ precios ticket)
                Nothing -> putStrLn "Ticket no encontrado"

            putStrLn "\nActualizando estado del ticket..."
            let updatedTicket = testTicket { estado = FirstBuy }
            updateTicket conn updatedTicket

            putStrLn "\nListando todos los tickets..."
            allTickets <- getAllTickets conn
            forM_ allTickets $ \ticket -> do
                putStrLn $ "\nTicket: " ++ ticketName ticket
                putStrLn $ "Estado: " ++ show (estado ticket)
                putStrLn $ "Precios:"
                putStrLn $ "  Compra 1: " ++ show (compra1 $ precios ticket)
                putStrLn $ "  Compra 2: " ++ show (compra2 $ precios ticket)
                putStrLn $ "  Venta 1: " ++ show (venta1 $ precios ticket)
                putStrLn $ "  Venta 2: " ++ show (venta2 $ precios ticket)
                putStrLn $ "  Take Profit: " ++ show (takeProfit $ precios ticket)
                putStrLn $ "  Stop Loss: " ++ show (stopLoss $ precios ticket)
                putStrLn $ "Puntas actuales:"
                putStrLn $ "  Compra: " ++ show (puntaCompra ticket)
                putStrLn $ "  Venta: " ++ show (puntaVenta ticket)

            -- Verificar datos guardados
            -- putStrLn "\nVerificando datos guardados..."
            
            -- putStrLn "\nÚltimo estado de cuenta:"
            -- latestCuentas <- getLatestEstadoCuenta conn
            -- forM_ latestCuentas $ \cuenta -> do
            --     putStrLn $ "Cuenta: " ++ T.unpack (cuentaNumero cuenta)
            --     putStrLn $ "Moneda: " ++ show (cuentaMoneda cuenta)
            --     putStrLn $ "Saldo disponible: " ++ show (cuentaDisponible cuenta)

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