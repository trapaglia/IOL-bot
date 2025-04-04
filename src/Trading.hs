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

-- Funciones placeholder para operaciones de trading
placeBuyOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeBuyOrder _ config ticket = do
    let cantidadCompra = floor (montoOperacion / puntaCompra ticket)
    currentTime <- getCurrentTimeArgentina
    let orden = OrdenRequest
            { ordenMercado = "bCBA"
            , ordenSimbolo = ticketName ticket
            , ordenCantidad = Just cantidadCompra
            , ordenPrecio = puntaCompra ticket
            , ordenPlazo = "t0"
            , ordenValidez = formatTime defaultTimeLocale "%FT%T.%3qZ" (addUTCTime 3600 currentTime)
            , ordenTipoOrden = Just "precioLimite"
            , ordenMonto = Nothing
            , ordenIdFuente = Nothing
            }
    
    putStrLn $ "  [ + ! + ] Comprando " ++ show cantidadCompra ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket)
    enviarOrdenCompra config orden

placeSellOrder :: Connection -> ApiConfig -> Ticket -> Int -> IO Bool
placeSellOrder _ config ticket cantidad = do
    currentTime <- getCurrentTimeArgentina
    let cantidadVenta = floor (fromIntegral cantidad * (0.82 :: Double)) :: Int
    let orden = OrdenRequest
            { ordenMercado = "bCBA"
            , ordenSimbolo = ticketName ticket
            , ordenCantidad = Just cantidadVenta
            , ordenPrecio = puntaVenta ticket
            , ordenPlazo = "t0"
            , ordenValidez = formatTime defaultTimeLocale "%FT%T.%3qZ" (addUTCTime 3600 currentTime)
            , ordenTipoOrden = Just "precioLimite"
            , ordenMonto = Nothing
            , ordenIdFuente = Nothing
            }
            
    putStrLn $ "  [ + ! + ] Vendiendo " ++ show cantidadVenta ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket)
    enviarOrdenVenta config orden

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
                                let sellAmount = floor (fromIntegral cantidad * (0.45 :: Double)) :: Int
                                let opAmount = if sellAmount > 0 then sellAmount else 1
                                success <- placeSellOrder conn config ticket opAmount
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
                                let sellAmount = floor (fromIntegral cantidad * (0.82 :: Double)) :: Int
                                let opAmount = if sellAmount > 0 then sellAmount else 1
                                success <- placeSellOrder conn config ticket opAmount
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
                                let sellAmount = floor (fromIntegral cantidad * (0.82 :: Double)) :: Int
                                let opAmount = if sellAmount > 0 then sellAmount else 1
                                success <- placeSellOrder conn config ticket opAmount
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
                                let sellAmount = cantidad
                                success <- placeSellOrder conn config ticket sellAmount
                                when success $ do
                                    let updatedTicket = ticket { estado = SecondSell }
                                    updateTicket conn updatedTicket
                                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        _ -> return () -- Para otros estados no hacemos nada por ahora
