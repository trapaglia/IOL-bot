{-# LANGUAGE OverloadedStrings #-}

module Trading
    ( processTicket
    , placeBuyOrder
    , placeSellOrder
    ) where

import Types (ApiConfig(..), Estado(..), Precios(..), Ticket(..), ComprarRequest(..), montoOperacion)
import Database.SQLite.Simple (Connection)
import Database (updateTicket)
import Control.Monad (when)
import Api (enviarOrdenCompra, compareMEP)

-- Funciones placeholder para operaciones de trading
placeBuyOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeBuyOrder _ config ticket = do
    let cantidadCompra = floor (montoOperacion / puntaCompra ticket)
        orden = ComprarRequest
            { compraMercado = "bCBA"
            , compraSimbolo = ticketName ticket
            , compraCantidad = cantidadCompra
            , compraPrecio = puntaCompra ticket
            , compraPlazo = "t0"
            , compraValidez = "immediateOrCancel"  -- Orden inmediata o cancelada
            , compraTipoOrden = "precioLimite"
            }
    
    putStrLn $ "  [ + ! + ] Comprando " ++ show cantidadCompra ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket)
    enviarOrdenCompra config orden

placeSellOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeSellOrder _ _ ticket = do
    putStrLn $ "[PLACEHOLDER] Vendiendo " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket)
    return True

-- Función para procesar un ticket según su estado y precios actuales
processTicket :: Connection -> ApiConfig -> Ticket -> IO ()
processTicket conn config ticket = do
    let targetPrices = precios ticket
    
    case estado ticket of
        Waiting -> when (puntaVenta ticket <= compra1 targetPrices) $ do
            (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
            if symbolDolarMEP > standardDolarMEP * 0.9
                then do
                    success <- placeBuyOrder conn config ticket
                    when success $ do
                        let updatedTicket = ticket { estado = FirstBuy }
                        updateTicket conn updatedTicket
                        putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a FirstBuy"
                else do
                    putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                    return ()

        FirstBuy -> do
            -- Check for second buy opportunity
            when (puntaVenta ticket <= compra2 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if symbolDolarMEP > standardDolarMEP * 0.9
                    then do
                        success <- placeBuyOrder conn config ticket
                        when success $ do
                            let updatedTicket = ticket { estado = SecondBuy }
                            updateTicket conn updatedTicket
                            putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondBuy"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()
            
            -- Check for first sell opportunity
            when (puntaCompra ticket >= venta1 targetPrices) $ do
                (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
                if symbolDolarMEP < standardDolarMEP * 1.1
                    then do
                        success <- placeSellOrder conn config ticket
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
                if symbolDolarMEP < standardDolarMEP * 1.1
                    then do
                        success <- placeSellOrder conn config ticket
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
                if symbolDolarMEP < standardDolarMEP * 1.1
                    then do
                        success <- placeSellOrder conn config ticket
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
                if symbolDolarMEP < standardDolarMEP * 1.1
                    then do
                        success <- placeSellOrder conn config ticket
                        when success $ do
                            let updatedTicket = ticket { estado = SecondSell }
                            updateTicket conn updatedTicket
                            putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"
                    else do
                        putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
                        return ()

        _ -> return () -- Para otros estados no hacemos nada por ahora
