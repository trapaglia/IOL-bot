{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)
import Types ( ApiConfig(..), Precios(..), Ticket(..)
            , CotizacionesResponse(..)
            , Instrumento(..), PuntaInstrumento(..)
            , instSimbolo, instPuntas, instVolumen, instCantidadOperaciones
            , precioCompra, precioVenta
            , EstadoCuenta(..), Cuenta(..), SaldoDetalle(..)
            )
import Api (getCredentials, getAllCotizaciones, getEstadoCuenta)
import Database
import Trading (processTicket)
import Database.SQLite.Simple (Connection, close)
import Control.Monad (forM_, forever)
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, MVar, tryTakeMVar)
import Control.Exception (catch, SomeException, fromException, AsyncException)
import Control.Concurrent.Async()
import Data.List (find)
import Text.Printf (printf)
import Utils (getCurrentTimeArgentina)

-- Función para actualizar todos los tickets
updateAllTickets :: Connection -> ApiConfig -> IO ()
updateAllTickets conn config = do
    putStrLn "\nActualizando tickets..."
    allTickets <- getAllTickets conn
    
    -- Obtener todas las cotizaciones de una vez
    maybeCotizaciones <- getAllCotizaciones config
    case maybeCotizaciones of
        Nothing -> putStrLn "Error al obtener cotizaciones"
        Just cotizaciones -> do
            now <- getCurrentTimeArgentina
            -- Para cada ticket, buscar su cotización en la respuesta
            forM_ allTickets $ \ticket -> do
                putStrLn $ "Actualizando " ++ ticketName ticket ++ "..."
                let maybeInstrumento = find (\inst -> instSimbolo inst == ticketName ticket) (titulos cotizaciones)
                case maybeInstrumento of
                    Just instrumento -> do
                        case instPuntas instrumento of
                            Just puntas -> do
                                let updatedTicket = ticket 
                                        { puntaCompra = precioCompra puntas
                                        , puntaVenta = precioVenta puntas
                                        , lastUpdate = now
                                        }
                                updateTicket conn updatedTicket
                                putStrLn $ "  Punta Compra: " ++ show (puntaCompra updatedTicket)
                                putStrLn $ "  Punta Venta: " ++ show (puntaVenta updatedTicket)
                                putStrLn $ "  Estado: " ++ show (estado updatedTicket)
                                putStrLn $ "  Precio de compra1: " ++ show (compra1 $ precios updatedTicket)
                                putStrLn $ "  Precio de venta1: " ++ show (venta1 $ precios updatedTicket)
                                putStrLn $ "  Precio de compra2: " ++ show (compra2 $ precios updatedTicket)
                                putStrLn $ "  Precio de venta2: " ++ show (venta2 $ precios updatedTicket)
                                putStrLn $ "  Volumen: " ++ printf "%.2f" (instVolumen instrumento)
                                putStrLn $ "  Operaciones: " ++ show (instCantidadOperaciones instrumento)

                                processTicket conn config updatedTicket
                            Nothing -> putStrLn $ "  No hay puntas disponibles para " ++ ticketName ticket
                    Nothing -> putStrLn $ "  No se encontró cotización para " ++ ticketName ticket

-- Bucle principal del programa
mainLoop :: Connection -> ApiConfig -> MVar () -> IO ()
mainLoop conn config stopMVar = do
    catch
        (updateAllTickets conn config)
        (\e -> putStrLn $ "Error en el bucle principal: " ++ show (e :: SomeException))
    
    putStrLn "\nEstado de cuenta:"
    maybeEstadoCuenta <- getEstadoCuenta config
    case maybeEstadoCuenta of
        Just ec -> do
            let cuentaPesos = head (cuentas ec)
            let cuentaDolares = (cuentas ec) !! 1
            putStrLn "\nCuenta Pesos"
            case saldos cuentaPesos of
                (s:_) -> do
                    putStrLn $ "Saldo: " ++ show (saldo s)
                    putStrLn $ "Comprometido: " ++ show (comprometido s)
                    putStrLn $ "Disponible: " ++ show (disponible s)
                [] -> putStrLn "No hay información de saldos"
            putStrLn "\nCuenta Dolares"
            case saldos cuentaDolares of
                (s:_) -> do
                    putStrLn $ "Saldo: " ++ show (saldo s)
                    putStrLn $ "Comprometido: " ++ show (comprometido s)
                    putStrLn $ "Disponible: " ++ show (disponible s)
                [] -> putStrLn "No hay información de saldos"
        Nothing -> putStrLn "No se pudo obtener el estado de cuenta"
    -- Verificar si debemos detener el programa
    stopped <- tryTakeMVar stopMVar
    case stopped of
        Just _ -> return ()
        Nothing -> do
            -- Esperar 60 segundos
            threadDelay (60 * 1000000)  -- threadDelay toma microsegundos
            -- mainLoop conn config stopMVar

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
        (forever $ mainLoop conn config stopMVar)
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
            
            -- maybeTicket <- getTicket conn "METR"
            -- when (maybeTicket == Nothing) $ do
            --     putStrLn "Creando ticket de ejemplo..."
            --     now <- getCurrentTimeArgentina
            --     let testTicket = Ticket
            --             { ticketName = "METR"
            --             , estado = SecondBuy
            --             , precios = Precios
            --                 { compra1 = 1820.0
            --                 , compra2 = 1820.0
            --                 , venta1 = 999999.0
            --                 , venta2 = 999999.0
            --                 , takeProfit = 999999.0
            --                 , stopLoss = 10.0
            --                 }
            --             , puntaCompra = 0.0
            --             , puntaVenta = 0.0
            --             , lastUpdate = now
            --             }
            --     insertTicket conn testTicket

            -- putStrLn "Obteniendo portafolio..."
            -- maybePortfolio <- getPortfolio config
            -- case maybePortfolio of
            --     Just portfolio -> do
            --         putStrLn "Portafolio obtenido correctamente"
            --         let tenencias = map (\asset -> (pack $ tituloSimbolo $ assetTitulo asset, round $ assetCantidad asset, assetUltimoPrecio asset)) (portfolioActivos portfolio)
            --         mapM_ (\(symbol, cantidad, precio) -> insertTenencia conn symbol cantidad precio) tenencias
            --     Nothing -> putStrLn "Error al obtener el portafolio"

            -- Iniciar bucle principal con manejo de interrupciones

            runMainLoop conn config
            
        _ -> do
            putStrLn "Error: Credenciales no encontradas en .env"
            close conn