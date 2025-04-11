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
import Api (getCredentials, getAllCotizaciones, getCEDEARsCotizaciones, getEstadoCuenta)
import Database (insertEstadoCuenta, updateTicket, connectDatabase, getAllTickets, getLastEstadoCuenta, DBEstadoCuenta(..))
import Trading (processTicket, monitorPuntas, monitorPuntasSymbols)
import Database (deleteTable)
import Database.SQLite.Simple (Connection, close)
import Control.Monad (forM_, forever, when)
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, MVar, tryTakeMVar)
import Control.Exception (catch, SomeException, fromException, AsyncException)
import Control.Concurrent.Async()
import Data.List (find)
import Data.Maybe (listToMaybe)
import Text.Printf (printf)
import Utils (getCurrentTimeArgentina)
import Data.IORef

-- Función para actualizar todos los tickets
updateAllTickets :: Connection -> ApiConfig -> Bool -> IO ()
updateAllTickets conn config verbose = do
    putStrLn "\nActualizando tickets..."
    
    -- Obtener todas las cotizaciones de una vez
    maybeCotizaciones <- getAllCotizaciones config
    maybeCotizacionesCedear <- getCEDEARsCotizaciones config
    
    -- Combinar las cotizaciones de ambas fuentes
    let allCotizaciones = case (maybeCotizaciones, maybeCotizacionesCedear) of
            (Just cot1, Just cot2) -> titulos cot1 ++ titulos cot2
            (Just cot1, Nothing) -> titulos cot1
            (Nothing, Just cot2) -> titulos cot2
            (Nothing, Nothing) -> []
            
    if null allCotizaciones
        then putStrLn "Error al obtener cotizaciones"
        else do
            now <- getCurrentTimeArgentina
            -- Para cada ticket, buscar su cotización en la respuesta combinada
            allTickets <- getAllTickets conn
            forM_ allTickets $ \ticket -> do
                putStrLn $ "Actualizando " ++ ticketName ticket ++ "..."
                let maybeInstrumento = find (\inst -> instSimbolo inst == ticketName ticket) allCotizaciones
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
                                when verbose $ do
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
                            Nothing -> when verbose $ putStrLn $ "  No hay puntas disponibles para " ++ ticketName ticket
                    Nothing -> when verbose $ putStrLn $ "  No se encontró cotización para " ++ ticketName ticket

-- Configuración del bucle principal
data LoopConfig = LoopConfig
    { monitorearPuntas :: Bool
    , simbolosMonitoreo :: [String]
    , monitorearTodos :: Bool
    }

-- Bucle principal del programa
mainLoop :: Connection -> ApiConfig -> MVar () -> IORef Int -> LoopConfig -> IO ()
mainLoop conn config stopMVar iterationRef loopConfig = do
    iteration <- readIORef iterationRef
    putStrLn $ "\nIteración " ++ show iteration
    
    
    putStrLn $ "\nIte mod 15 = " ++ show (mod iteration 15) ++ " Estado de cuenta:"
    if (iteration `mod` 15 == 0) 
    then do
        catch
            (updateAllTickets conn config True)
            (\e -> putStrLn $ "Error en el bucle principal: " ++ show (e :: SomeException))
        -- Obtener estado de cuenta desde la API cada 15 iteraciones
        maybeEstadoCuenta <- getEstadoCuenta config
        case maybeEstadoCuenta of
            Just ec -> do
                let cuentaPesos = head (cuentas ec)
                let cuentaDolares = (cuentas ec) !! 1
                
                deleteTable conn "estado_cuenta"
                -- Guardar en la base de datos
                insertEstadoCuenta conn cuentaPesos
                insertEstadoCuenta conn cuentaDolares
                
                putStrLn "\nCuenta Pesos (desde API)"
                case saldos cuentaPesos of
                    (s:_) -> do
                        putStrLn $ "Saldo: " ++ show (saldo s)
                        putStrLn $ "Comprometido: " ++ show (comprometido s)
                        putStrLn $ "Disponible: " ++ show (disponible s)
                    [] -> putStrLn "No hay información de saldos"
                putStrLn "\nCuenta Dolares (desde API)"
                case saldos cuentaDolares of
                    (s:_) -> do
                        putStrLn $ "Saldo: " ++ show (saldo s)
                        putStrLn $ "Comprometido: " ++ show (comprometido s)
                        putStrLn $ "Disponible: " ++ show (disponible s)
                    [] -> putStrLn "No hay información de saldos"
            Nothing -> putStrLn "No se pudo obtener el estado de cuenta"
    else do
        catch
            (updateAllTickets conn config False)
            (\e -> putStrLn $ "Error en el bucle principal: " ++ show (e :: SomeException))
        -- Obtener último estado de cuenta desde la base de datos
        cuentas_api <- getLastEstadoCuenta conn
        case cuentas_api of
            [] -> putStrLn "No hay información de cuentas en la base de datos"
            _ -> do
                let maybeCuentaPesos = listToMaybe [c | c <- cuentas_api, cuentaMoneda c == "peso_Argentino"]
                let maybeCuentaDolares = listToMaybe [c | c <- cuentas_api, cuentaMoneda c == "dolar_Estadounidense"]
                
                putStrLn "\nCuenta Pesos (desde DB)"
                case maybeCuentaPesos of
                    Just cuenta -> do
                        putStrLn $ "Saldo: " ++ show (cuentaSaldo cuenta)
                        putStrLn $ "Comprometido: " ++ show (cuentaComprometido cuenta)
                        putStrLn $ "Disponible: " ++ show (cuentaDisponible cuenta)
                    Nothing -> putStrLn "No se encontró la cuenta en pesos en la base de datos"
                
                putStrLn "\nCuenta Dolares (desde DB)"
                case maybeCuentaDolares of
                    Just cuenta -> do
                        putStrLn $ "Saldo: " ++ show (cuentaSaldo cuenta)
                        putStrLn $ "Comprometido: " ++ show (cuentaComprometido cuenta)
                        putStrLn $ "Disponible: " ++ show (cuentaDisponible cuenta)
                    Nothing -> putStrLn "No se encontró la cuenta en dólares en la base de datos"

    -- Monitoreo de puntas si está habilitado
    when (monitorearPuntas loopConfig) $ do
        putStrLn "\nMonitoreando puntas..."
        if monitorearTodos loopConfig
        then do
            putStrLn "Monitoreando puntas de todos los tickets..."
            monitorPuntas config
        else do
            let symbols = simbolosMonitoreo loopConfig
            putStrLn $ "Monitoreando puntas de símbolos específicos: " ++ show symbols
            monitorPuntasSymbols config symbols

    putStrLn "\nContenido del archivo logs/ordenes_ejecutadas.log:"
    cont <- readFile "logs/ordenes_ejecutadas.log"
    putStrLn cont

    -- Verificar si debemos detener el programa
    stopped <- tryTakeMVar stopMVar
    case stopped of
        Just _ -> return ()
        Nothing -> do
            -- Esperar 60 segundos
            threadDelay (60 * 1000000)  -- threadDelay toma microsegundos
            modifyIORef' iterationRef (+1)

-- Función para ejecutar el bucle principal con manejo de interrupciones
runMainLoop :: Connection -> ApiConfig -> LoopConfig -> IO ()
runMainLoop conn config loopConfig = do
    putStrLn "\nIniciando bucle principal... (Presiona Ctrl+C para detener)"
    stopMVar <- newEmptyMVar
    iterationRef <- newIORef 0  -- Crear referencia para la iteración
    
    -- Configurar manejador de Ctrl+C
    let cleanup = do
            putStrLn "\nDeteniendo el programa..."
            putMVar stopMVar ()
            putStrLn "Cerrando conexión a la base de datos..."
            close conn
            putStrLn "¡Hasta luego!"
    
    -- Ejecutar el bucle principal con manejo de excepciones
    catch 
        (forever $ mainLoop conn config stopMVar iterationRef loopConfig)
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
            putStrLn "Credenciales cargadas correctamente"
            let config = ApiConfig user pass
            
            -- Menú de opciones
            putStrLn "\nSelecciona una opción:"
            putStrLn "1. Iniciar bucle principal (sin monitoreo de puntas)"
            putStrLn "2. Iniciar bucle principal con monitoreo de puntas (todos los tickets)"
            putStrLn "3. Iniciar bucle principal con monitoreo de puntas (símbolos específicos)"
            putStrLn "4. Actualizar tickets una vez"
            putStrLn "5. Monitorear puntas una vez (todos los tickets)"
            putStrLn "6. Monitorear puntas una vez (símbolos específicos)"
            putStrLn "7. Salir"
            
            putStr "Opción: "
            option <- getLine
            
            case option of
                "1" -> do
                    let loopConfig = LoopConfig False [] False
                    runMainLoop conn config loopConfig
                "2" -> do
                    let loopConfig = LoopConfig True [] True
                    runMainLoop conn config loopConfig
                "3" -> do
                    putStrLn "Ingresa los símbolos separados por comas (ej: AL30,GD30,GGAL):"
                    symbolsInput <- getLine
                    let symbols = words $ map (\c -> if c == ',' then ' ' else c) symbolsInput
                    putStrLn $ "Monitoreando puntas de: " ++ show symbols
                    let loopConfig = LoopConfig True symbols False
                    runMainLoop conn config loopConfig
                "4" -> do
                    updateAllTickets conn config True
                    putStrLn "Tickets actualizados. Saliendo..."
                "5" -> do
                    putStrLn "Monitoreando puntas de todos los tickets..."
                    monitorPuntas config
                    putStrLn "Monitoreo completado. Saliendo..."
                "6" -> do
                    putStrLn "Ingresa los símbolos separados por comas (ej: AL30,GD30,GGAL):"
                    symbolsInput <- getLine
                    let symbols = words $ map (\c -> if c == ',' then ' ' else c) symbolsInput
                    putStrLn $ "Monitoreando puntas de: " ++ show symbols
                    monitorPuntasSymbols config symbols
                    putStrLn "Monitoreo completado. Saliendo..."
                "7" -> putStrLn "Saliendo..."
                _ -> do
                    putStrLn "Opción no válida. Saliendo..."
            
            -- Cerrar conexión a la base de datos
            close conn
        _ -> putStrLn "Error al cargar las credenciales. Verifica el archivo .env"