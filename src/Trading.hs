{-# LANGUAGE OverloadedStrings #-}

module Trading
    ( processTicket
    , placeBuyOrder
    , placeSellOrder
    ) where

import Types (ApiConfig(..), Estado(..), Precios(..), Ticket(..), montoOperacion, DolarMEP(..))
import Database.SQLite.Simple (Connection)
import Database (updateTicket)
import Control.Monad (when)
import Api (getDolarMEP, enviarOrdenCompra, enviarOrdenVenta, OrdenRequest(..))
import Utils (getCurrentTimeArgentina)
import Helper (getCantidadPortfolio, compareMEP)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (addUTCTime)
import Data.Maybe (fromMaybe)


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
        , ordenPlazo = "t1"
        , ordenValidez = formatTime defaultTimeLocale "%FT%T.%3qZ" (addUTCTime 3600 currentTime)
        , ordenTipoOrden = Just "precioLimite"
        , ordenMonto = Nothing
        , ordenIdFuente = Nothing
        }

placeBuyOrder :: Connection -> ApiConfig -> Ticket -> IO Bool
placeBuyOrder _ config ticket = do
    (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
    let logFileName = "logs/ordenes_ejecutadas.log"
    if (symbolDolarMEP > standardDolarMEP * 0.9) || symbolDolarMEP == 1
    then do
        -- let operacionPesos = True
        --         let ticketName ticket = ticketName ticket ++ "D"
        --         let symbolDolar = getCotizacion config (ticketName ticket)
        --         let priceDolar = (puntaCompra ticket / standardDolarMEP) + 1.0
        --         putStrLn $ "  [ + !] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ show (symbolDolarMEP/standardDolarMEP) ++ ")"
        --         putStrLn $ "  [ + !] Dolar MEP: " ++ show symbolDolarMEP ++ " AL30: " ++ show standardDolarMEP

        let cantidadCompra = montoOperacion / puntaVenta ticket
        orden <- createOrdenRequest ticket cantidadCompra
        putStrLn $ "  [ + ! + ] Comprando " ++ show (ordenCantidad orden) ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaVenta ticket) ++ " pesos."
        putStrLn $ " [I] Total gastado " ++ show (fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaVenta ticket) ++ " pesos."
        rsp <- enviarOrdenCompra config orden
        currentTime <- getCurrentTimeArgentina
        if rsp then do
            putStrLn "  [ + ! + ] Compra exitosa"
            let opAmountDolares = fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaVenta ticket / standardDolarMEP
            let ticketPriceDolares = puntaVenta ticket / standardDolarMEP
            let ordenLog = "Compra de " ++ show (fromMaybe 0 (ordenCantidad orden)) ++ " de " ++ ticketName ticket ++ 
                    " a " ++ show (puntaVenta ticket) ++ " pesos.\n" ++
                    " con plazo " ++ ordenPlazo orden ++ " y validez " ++ ordenValidez orden ++
                    " a las " ++ formatTime defaultTimeLocale "%H:%M:%S" currentTime ++ "\n" ++
                    " Total: " ++ show opAmountDolares ++ " dolares, " ++ show (fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaCompra ticket) ++ " pesos.\n" ++
                    " Precio del ticket: " ++ show ticketPriceDolares ++ " dolares.\n" ++
                    " Con un dolar MEP de " ++ show standardDolarMEP
            appendFile logFileName ordenLog
            return True
        else do
            putStrLn "  [ - ! - ] Compra fallida"
            return False
    else do
        -- let operacionPesos = False
        let ordenLog = "  [ + E] Ticket " ++ ticketName ticket ++ " no cumple con la condición de compra (Relacion Dolar MEP = " ++ 
                       show (symbolDolarMEP/standardDolarMEP) ++ ")\n" ++
                       "  [ + E] Dolar MEP: " ++ show symbolDolarMEP ++ " AL30: " ++ show standardDolarMEP
        appendFile logFileName ordenLog
        putStrLn ordenLog
        return False


placeSellOrder :: Connection -> ApiConfig -> Ticket -> Double -> IO Bool
placeSellOrder _ config ticket cantidad = do
    (symbolDolarMEP, standardDolarMEP) <- compareMEP config (ticketName ticket)
    let logFileName = "logs/ordenes_ejecutadas.log"
    if (symbolDolarMEP < standardDolarMEP * 1.1) || symbolDolarMEP == 1
    then do
        -- let operacionPesos = True
        let cantidadVenta = cantidad * (0.82 :: Double)
        orden <- createOrdenRequest ticket cantidadVenta
        putStrLn $ "  [ + ! + ] Vendiendo " ++ show cantidadVenta ++ " de " ++ ticketName ticket ++ " a " ++ show (puntaCompra ticket) ++ " pesos."
        putStrLn $ " [I] Total recibido " ++ show (fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaCompra ticket) ++ " pesos."
        rsp <- enviarOrdenVenta config orden
        currentTime <- getCurrentTimeArgentina
        if rsp then do
            putStrLn "  [ + ! + ] Venta exitosa"
            let opAmountDolares = fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaCompra ticket / standardDolarMEP
            let ticketPriceDolares = puntaCompra ticket / standardDolarMEP
            let ordenLog = "Venta de " ++ show (fromMaybe 0 (ordenCantidad orden)) ++ " de " ++ ticketName ticket ++ 
                        " a " ++ show (puntaCompra ticket) ++ " pesos.\n" ++
                        " con plazo " ++ ordenPlazo orden ++ " y validez " ++ ordenValidez orden ++
                        " a las " ++ formatTime defaultTimeLocale "%H:%M:%S" currentTime ++ "\n" ++
                        " Total: " ++ show opAmountDolares ++ " dolares, " ++ show (fromIntegral (fromMaybe 0 (ordenCantidad orden)) * puntaVenta ticket) ++ " pesos.\n" ++
                        " Precio del ticket: " ++ show ticketPriceDolares ++ " dolares"
            appendFile logFileName ordenLog
            return True
        else do
            putStrLn "  [ - ! - ] Venta fallida"
            return False
    else do
        -- let operacionPesos = False
        let ordenLog = "  [ + E] Ticket " ++ ticketName ticket ++ " no cumple con la condición de venta (Relacion Dolar MEP = " ++ 
                        show (symbolDolarMEP/standardDolarMEP) ++ ")\n" ++
                        "  [ + E] Dolar MEP: " ++ show symbolDolarMEP ++ " AL30: " ++ show standardDolarMEP
        appendFile logFileName ordenLog
        putStrLn ordenLog
        return False

-- Función para procesar un ticket según su estado y precios actuales
processTicket :: Connection -> ApiConfig -> Ticket -> IO ()
processTicket conn config ticket = do
    let targetPrices = precios ticket
    
    case estado ticket of
        Waiting -> when (puntaVenta ticket <= compra1 targetPrices) $ do
            success <- placeBuyOrder conn config ticket
            when success $ do
                let updatedTicket = ticket { estado = FirstBuy }
                updateTicket conn updatedTicket
                putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a FirstBuy"

        FirstBuy -> do
            -- Check for second buy opportunity
            when (puntaVenta ticket <= compra2 targetPrices) $ do
                success <- placeBuyOrder conn config ticket
                when success $ do
                    let updatedTicket = ticket { estado = SecondBuy }
                    updateTicket conn updatedTicket
                    putStrLn $ "  [I] Ticket " ++ ticketName ticket ++ " actualizado a SecondBuy"
            
            -- Check for first sell opportunity
            when (puntaCompra ticket >= venta1 targetPrices) $ do
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

        SecondBuy -> 
            when (puntaCompra ticket >= venta1 targetPrices) $ do
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

        FirstSell -> 
            when (puntaCompra ticket >= venta2 targetPrices) $ do
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

        SecondSell -> 
            when (puntaCompra ticket >= takeProfit targetPrices) $ do
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

        _ -> return () -- Para otros estados no hacemos nada por ahora
