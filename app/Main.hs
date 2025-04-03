{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types (ApiConfig(..), EstadoCuenta(..), Estado(..), Precios(..), Ticket(..))
import Api (getCredentials, getCotizacion, getEstadoCuenta, updateTicketMarketData)
import Database
import Utils (getCurrentTimeArgentina)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close)
import Control.Monad (forM_, forever, when)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Concurrent.Async (withAsync, waitCatch)

-- Lista de símbolos para obtener cotizaciones
symbols :: [T.Text]
symbols = ["GGAL"] --"PAMP", "YPF", "BBAR"]

-- Función para actualizar todos los tickets
updateAllTickets :: Connection -> ApiConfig -> IO ()
updateAllTickets conn config = do
    putStrLn "\nActualizando tickets..."
    allTickets <- getAllTickets conn
    forM_ allTickets $ \ticket -> do
        putStrLn $ "Actualizando " ++ ticketName ticket ++ "..."
        maybeUpdatedTicket <- updateTicketMarketData config ticket
        case maybeUpdatedTicket of
            Just updatedTicket -> do
                updateTicket conn updatedTicket
                putStrLn $ "  Compra: " ++ show (puntaCompra updatedTicket)
                putStrLn $ "  Venta: " ++ show (puntaVenta updatedTicket)
            Nothing -> putStrLn $ "  Error al actualizar " ++ ticketName ticket

-- Bucle principal del programa
mainLoop :: Connection -> ApiConfig -> IO ()
mainLoop conn config = forever $ do
    catch
        (updateAllTickets conn config)
        (\e -> putStrLn $ "Error en el bucle principal: " ++ show (e :: SomeException))
    
    -- Esperar 30 segundos
    threadDelay (30 * 1000000)  -- threadDelay toma microsegundos

-- Función para ejecutar el bucle principal con manejo de interrupciones
runMainLoop :: Connection -> ApiConfig -> IO ()
runMainLoop conn config = do
    putStrLn "\nIniciando bucle principal... (Presiona Ctrl+C para detener)"
    withAsync (mainLoop conn config) $ \asyncHandle -> do
        result <- waitCatch asyncHandle
        case result of
            Left e -> putStrLn $ "\nPrograma interrumpido: " ++ show e
            Right _ -> putStrLn "\nPrograma finalizado normalmente"
    putStrLn "Cerrando conexión a la base de datos..."
    close conn
    putStrLn "¡Hasta luego!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- Inicializar base de datos
    putStrLn "Conectando a la base de datos..."
    conn <- connectDatabase

    -- Obtener credenciales
    (maybeUsername, maybePassword) <- getCredentials
    case (maybeUsername, maybePassword) of
        (Just user, Just pass) -> do
            putStrLn "Credentials OK!\n"
            let config = ApiConfig user pass
            
            -- Crear ticket de ejemplo si no existe
            maybeTicket <- getTicket conn "GGAL"
            when (maybeTicket == Nothing) $ do
                putStrLn "Creando ticket de ejemplo..."
                now <- getCurrentTimeArgentina
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
                        , puntaCompra = 0.0
                        , puntaVenta = 0.0
                        , lastUpdate = now
                        }
                insertTicket conn testTicket

            -- Iniciar bucle principal con manejo de interrupciones
            runMainLoop conn config
            
        _ -> do
            putStrLn "Error: Credenciales no encontradas en .env"
            close conn