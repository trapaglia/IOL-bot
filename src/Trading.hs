{-# LANGUAGE OverloadedStrings #-}

module Trading
    ( processTicket
    , placeBuyOrder
    , placeSellOrder
    ) where

import Types (ApiConfig(..), Estado(..), Precios(..), Ticket(..))
import Database (Connection, updateTicket)
import Control.Monad (when)

-- Funciones placeholder para operaciones de trading
placeBuyOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeBuyOrder _ _ ticket = do
    putStrLn $ "[PLACEHOLDER] Comprando " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket)
    return True

placeSellOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeSellOrder _ _ ticket = do
    putStrLn $ "[PLACEHOLDER] Vendiendo " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket)
    return True

-- Función para procesar un ticket según su estado y precios actuales
processTicket :: Connection -> ApiConfig -> Ticket -> IO ()
processTicket conn config ticket = do
    let currentPrice = puntaCompra ticket
        targetPrices = precios ticket
    
    case estado ticket of
        Waiting -> when (currentPrice <= compra1 targetPrices) $ do
            success <- placeBuyOrder conn config ticket
            when success $ do
                let updatedTicket = ticket { estado = FirstBuy }
                updateTicket conn updatedTicket
                putStrLn $ "Ticket " ++ ticketName ticket ++ " actualizado a FirstBuy"

        FirstBuy -> do
            -- Check for second buy opportunity
            when (currentPrice <= compra2 targetPrices) $ do
                success <- placeBuyOrder conn config ticket
                when success $ do
                    let updatedTicket = ticket { estado = SecondBuy }
                    updateTicket conn updatedTicket
                    putStrLn $ "Ticket " ++ ticketName ticket ++ " actualizado a SecondBuy"
            
            -- Check for first sell opportunity
            when (puntaVenta ticket >= venta1 targetPrices) $ do
                success <- placeSellOrder conn config ticket
                when success $ do
                    let updatedTicket = ticket { estado = FirstSell }
                    updateTicket conn updatedTicket
                    putStrLn $ "Ticket " ++ ticketName ticket ++ " actualizado a FirstSell"

        SecondBuy -> 
            when (puntaVenta ticket >= venta1 targetPrices) $ do
                success <- placeSellOrder conn config ticket
                when success $ do
                    let updatedTicket = ticket { estado = SecondSell }
                    updateTicket conn updatedTicket
                    putStrLn $ "Ticket " ++ ticketName ticket ++ " actualizado a SecondSell"

        _ -> return () -- Para otros estados no hacemos nada por ahora
