{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types (ApiConfig(..), Estado(..), Precios(..), Ticket(..), PortfolioAsset(..), AssetTitle(..), PortfolioResponse(..))
import Api (getCredentials, updateTicketMarketData, getPortfolio)
import Database
import Trading (processTicket)
import Utils (getCurrentTimeArgentina)
import Data.Text (pack)
import Database.SQLite.Simple (Connection, close)
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, MVar, tryTakeMVar)
import Control.Exception (catch, SomeException, fromException, AsyncException)
import Control.Concurrent.Async()
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
                putStrLn $ "  Punta Compra: " ++ show (puntaCompra updatedTicket)
                putStrLn $ "  Punta Venta: " ++ show (puntaVenta updatedTicket)
                putStrLn $ "  Estado: " ++ show (estado updatedTicket)
                putStrLn $ "  Precio de compra1: " ++ show (compra1 $ precios updatedTicket)
                putStrLn $ "  Precio de venta1: " ++ show (venta1 $ precios updatedTicket)
                putStrLn $ "  Precio de compra2: " ++ show (compra2 $ precios updatedTicket)
                putStrLn $ "  Precio de venta2: " ++ show (venta2 $ precios updatedTicket)

                processTicket conn config updatedTicket
            Nothing -> putStrLn $ "  Error al actualizar " ++ ticketName ticket

-- Bucle principal del programa
mainLoop :: Connection -> ApiConfig -> MVar () -> IO ()
mainLoop conn config stopMVar = do
    catch
        (updateAllTickets conn config)
        (\e -> putStrLn $ "Error en el bucle principal: " ++ show (e :: SomeException))
    
    -- Verificar si debemos detener el programa
    stopped <- tryTakeMVar stopMVar
    case stopped of
        Just _ -> return ()
        Nothing -> do
            -- Esperar 30 segundos
            threadDelay (30 * 1000000)  -- threadDelay toma microsegundos
            mainLoop conn config stopMVar

-- Función para ejecutar el bucle principal con manejo de interrupciones
runMainLoop :: Connection -> ApiConfig -> IO ()
runMainLoop conn config = do
    putStrLn "\nIniciando bucle principal... (Presiona Ctrl+C para detener)"
    stopMVar <- newEmptyMVar
    
    -- Configurar manejador de Ctrl+C
    let cleanup = do
            putStrLn "\nDeteniendo el programa..."
            putMVar stopMVar ()
            putStrLn "Cerrando conexión a la base de datos..."
            close conn
            putStrLn "¡Hasta luego!"
    
    -- Ejecutar el bucle principal con manejo de excepciones
    catch 
        (mainLoop conn config stopMVar)
        (\e -> do
            case e of
                -- Si es una interrupción por Ctrl+C
                _ | Just (_ :: AsyncException) <- fromException e -> do
                    cleanup
                -- Para otras excepciones
                _ -> do
                    putStrLn $ "Error inesperado: " ++ show (e :: SomeException)
                    cleanup
        )

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- Inicializar base de datos
    putStrLn "Conectando a la base de datos..."
    -- conn <- resetDatabase
    conn <- connectDatabase

    -- Obtener credenciales
    (maybeUsername, maybePassword) <- getCredentials
    case (maybeUsername, maybePassword) of
        (Just user, Just pass) -> do
            putStrLn "Credentials OK!\n"
            let config = ApiConfig user pass
            
            -- Crear ticket de ejemplo si no existe
            -- maybeTicket <- getTicket conn "GGAL"
            -- when (maybeTicket == Nothing) $ do
            --     putStrLn "Creando ticket de ejemplo..."
            --     now <- getCurrentTimeArgentina
            --     let testTicket = Ticket
            --             { ticketName = "GGAL"
            --             , estado = FirstBuy
            --             , precios = Precios
            --                 { compra1 = 7500
            --                 , compra2 = 6650
            --                 , venta1 = 8000
            --                 , venta2 = 8500
            --                 , takeProfit = 9000
            --                 , stopLoss = 5000.0
            --                 }
            --             , puntaCompra = 0.0
            --             , puntaVenta = 0.0
            --             , lastUpdate = now
            --             }
            --     insertTicket conn testTicket

            maybeTicket <- getTicket conn "BBAR"
            when (maybeTicket == Nothing) $ do
                putStrLn "Creando ticket de ejemplo..."
                now <- getCurrentTimeArgentina
                let testTicket = Ticket
                        { ticketName = "BBAR"
                        , estado = FirstBuy
                        , precios = Precios
                            { compra1 = 7500.0
                            , compra2 = 6650.0
                            , venta1 = 999999.0
                            , venta2 = 999999.0
                            , takeProfit = 999999.0
                            , stopLoss = 1000.0
                            }
                        , puntaCompra = 0.0
                        , puntaVenta = 0.0
                        , lastUpdate = now
                        }
                insertTicket conn testTicket

            putStrLn "Obteniendo portafolio..."
            maybePortfolio <- getPortfolio config
            case maybePortfolio of
                Just portfolio -> do
                    putStrLn "Portafolio obtenido correctamente"
                    let tenencias = map (\asset -> (pack $ tituloSimbolo $ assetTitulo asset, round $ assetCantidad asset, assetUltimoPrecio asset)) (portfolioActivos portfolio)
                    mapM_ (\(symbol, cantidad, precio) -> insertTenencia conn symbol cantidad precio) tenencias
                Nothing -> putStrLn "Error al obtener el portafolio"

            -- Iniciar bucle principal con manejo de interrupciones
            runMainLoop conn config
            
        _ -> do
            putStrLn "Error: Credenciales no encontradas en .env"
            close conn