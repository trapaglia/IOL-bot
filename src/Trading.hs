{-# LANGUAGE OverloadedStrings #-}

module Trading
    ( processTicket
    , placeBuyOrder
    , placeSellOrder
    ) where

import Types (ApiConfig(..), Estado(..), Precios(..), Ticket(..), montoOperacion)
import Database.SQLite.Simple (Connection)
import Database (updateTicket)
import Control.Monad (when)
import Api (enviarOrdenCompra, compareMEP, enviarOrdenVenta, OrdenRequest(..), getCantidadPortfolio)
import Utils (getCurrentTimeArgentina)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (addUTCTime)

createOrdenRequest :: Ticket -> Double -> IO OrdenRequest
createOrdenRequest ticket cantidad = do
    currentTime <- getCurrentTimeArgentina
    let amount = floor cantidad :: Int
    let opAmount = if amount > 0 then amount else 1
    return OrdenRequest
        { ordenMercado = "bCBA"
        , ordenSimbolo = ticketName ticket
        , ordenCantidad = Just opAmount
        , ordenPrecio = puntaVenta ticket
        , ordenPlazo = "t0"
        , ordenValidez = formatTime defaultTimeLocale "%FT%T.%3qZ" (addUTCTime 3600 currentTime)
        , ordenTipoOrden = Just "precioLimite"
        , ordenMonto = Nothing
        , ordenIdFuente = Nothing
        }

placeBuyOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeBuyOrder _ config ticket = do
    let cantidadCompra = montoOperacion / puntaCompra ticket
    orden <- createOrdenRequest ticket cantidadCompra
    putStrLn $ "  [ + ! + ] Comprando " ++ show (ordenCantidad orden) ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket)
    putStrLn $ " [I] Total gastado " ++ show (fromIntegral (maybe 0 id (ordenCantidad orden)) * puntaCompra ticket)
    rsp <- enviarOrdenCompra config orden
    currentTime <- getCurrentTimeArgentina
    case rsp of
        True -> do
            putStrLn "  [ + ! + ] Compra exitosa"
            let logFileName = "ordenes_ejecutadas.log"
            let ordenLog = "Compra de " ++ show cantidadCompra ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket) ++
                           " con plazo " ++ ordenPlazo orden ++ " y validez " ++ ordenValidez orden ++
                           " a las " ++ formatTime defaultTimeLocale "%H:%M:%S" currentTime ++ "\n"
            appendFile logFileName ordenLog
        False -> putStrLn "  [ - ! - ] Compra fallida"
    return rsp

placeSellOrder :: Connection -> ApiConfig -> Ticket -> Double -> IO Bool
placeSellOrder _ config ticket cantidad = do
    let cantidadVenta = cantidad * (0.82 :: Double)
    orden <- createOrdenRequest ticket cantidadVenta
    putStrLn $ "  [ + ! + ] Vendiendo " ++ show cantidadVenta ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket)
    putStrLn $ " [I] Total recibido " ++ show (fromIntegral (maybe 0 id (ordenCantidad orden)) * puntaVenta ticket)
    rsp <- enviarOrdenVenta config orden
    currentTime <- getCurrentTimeArgentina
    case rsp of
        True -> do
            putStrLn "  [ + ! + ] Venta exitosa"
            let logFileName = "ordenes_ejecutadas.log"
            let ordenLog = "Venta de " ++ show cantidadVenta ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket) ++
                           " con plazo " ++ ordenPlazo orden ++ " y validez " ++ ordenValidez orden ++
                           " a las " ++ formatTime defaultTimeLocale "%H:%M:%S" currentTime ++ "\n"
            appendFile logFileName ordenLog
        False -> putStrLn "  [ - ! - ] Venta fallida"
    return rsp

-- Función para procesar un ticket según su estado y precios actuales
processTicket :: Connection -> ApiConfig -> Ticket -> IO ()
processTicket conn config ticket = do
    let targetPrices = precios ticket
    
    case estado ticket of
        Waiting -> when (puntaVenta ticket <= compra1 targetPrices) $ do
            (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
            if (symbolDolarMEP > standardDolarMEP * 0.9) || symbolDolarMEP == 0
                then do
                    success <- placeBuyOrder conn config ticket
                    when success $ do
                        let updatedTicket = ticket { estado = FirstBuy }
                        updateTicket conn updatedTicket
                        putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a FirstBuy"
                else do
                    putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                    putStrLn $ "  [ + !] Dolar MEP: " ++ show symbolDolarMEP ++ " AL30: " ++ show standardDolarMEP
                    return ()

        FirstBuy -> do
            -- Check for second buy opportunity
            when (puntaVenta ticket <= compra2 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if (symbolDolarMEP > standardDolarMEP * 0.9) || symbolDolarMEP == 0
                    then do
                        success <- placeBuyOrder conn config ticket
                        when success $ do
                            let updatedTicket = ticket { estado = SecondBuy }
                            updateTicket conn updatedTicket
                            putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondBuy"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        putStrLn $ "  [ + !] Dolar MEP: " ++ show symbolDolarMEP ++ " AL30: " ++ show standardDolarMEP
                        return ()
            
            -- Check for first sell opportunity
            when (puntaCompra ticket >= venta1 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if (symbolDolarMEP < standardDolarMEP * 1.1) || symbolDolarMEP == 0
                    then do
                        maybeCantidad <- getCantidadPortfolio config (ticketName ticket)
                        case maybeCantidad of
                            Nothing -> do
                                putStrLn $ "  [ - ! - ] No se pudo obtener la cantidad disponible de " ++ ticketName ticket
                                return ()
                            Just cantidad -> do
                                let sellAmount = fromIntegral cantidad * (0.45 :: Double)
                                success <- placeSellOrder conn config ticket sellAmount
                                when success $ do
                                    let updatedTicket = ticket { estado = FirstSell }
                                    updateTicket conn updatedTicket
                                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a FirstSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        SecondBuy -> 
            when (puntaCompra ticket >= venta1 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if (symbolDolarMEP < standardDolarMEP * 1.1) || symbolDolarMEP == 0
                    then do
                        maybeCantidad <- getCantidadPortfolio config (ticketName ticket)
                        case maybeCantidad of
                            Nothing -> do
                                putStrLn $ "  [ - ! - ] No se pudo obtener la cantidad disponible de " ++ ticketName ticket
                                return ()
                            Just cantidad -> do
                                let sellAmount = fromIntegral cantidad * (0.82 :: Double)
                                success <- placeSellOrder conn config ticket sellAmount
                                when success $ do
                                    let updatedTicket = ticket { estado = SecondSell }
                                    updateTicket conn updatedTicket
                                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        FirstSell -> 
            when (puntaCompra ticket >= venta2 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if (symbolDolarMEP < standardDolarMEP * 1.1) || symbolDolarMEP == 0
                    then do
                        maybeCantidad <- getCantidadPortfolio config (ticketName ticket)
                        case maybeCantidad of
                            Nothing -> do
                                putStrLn $ "  [ - ! - ] No se pudo obtener la cantidad disponible de " ++ ticketName ticket
                                return ()
                            Just cantidad -> do
                                let sellAmount = fromIntegral cantidad * (0.82 :: Double)
                                success <- placeSellOrder conn config ticket sellAmount
                                when success $ do
                                    let updatedTicket = ticket { estado = SecondSell }
                                    updateTicket conn updatedTicket
                                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        SecondSell -> 
            when (puntaCompra ticket >= takeProfit targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if (symbolDolarMEP < standardDolarMEP * 1.1) || symbolDolarMEP == 0
                    then do
                        maybeCantidad <- getCantidadPortfolio config (ticketName ticket)
                        case maybeCantidad of
                            Nothing -> do
                                putStrLn $ "  [ - ! - ] No se pudo obtener la cantidad disponible de " ++ ticketName ticket
                                return ()
                            Just cantidad -> do
                                let sellAmount = fromIntegral cantidad
                                success <- placeSellOrder conn config ticket sellAmount
                                when success $ do
                                    let updatedTicket = ticket { estado = SecondSell }
                                    updateTicket conn updatedTicket
                                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        _ -> return () -- Para otros estados no hacemos nada por ahora
