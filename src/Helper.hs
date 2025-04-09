module Helper (compareMEP, getCantidadPortfolio) where
import Api (getDolarMEP, getPortfolio)
import Types (ApiConfig, DolarMEP(..), PortfolioResponse(..), PortfolioAsset(..), AssetTitle(..))
import Data.List (find)

-- Función para comparar el dólar MEP con el AL30
compareMEP :: ApiConfig -> String -> IO (Double, Double)
compareMEP config symbol = do
    symbolDolarMEP <- getDolarMEP config symbol
    standardDolarMEP <- getDolarMEP config "AL30"
    case (symbolDolarMEP, standardDolarMEP) of
        (Just (DolarMEP symbolMEP), Just (DolarMEP standardMEP)) -> return (symbolMEP, standardMEP)
        (Nothing, Just (DolarMEP standardMEP)) -> return(1.0, standardMEP)
        _ -> return (1.0, 1.0)

-- Función para obtener la cantidad de un símbolo específico del portafolio
getCantidadPortfolio :: ApiConfig -> String -> IO (Maybe Int)
getCantidadPortfolio config symbol = do
    maybePortfolio <- getPortfolio config
    case maybePortfolio of
        Nothing -> return Nothing
        Just portfolio -> do
            let activo = find (\asset -> tituloSimbolo (assetTitulo asset) == symbol) (portfolioActivos portfolio)
            case activo of
                Nothing -> do
                    putStrLn $ "No se encontró el símbolo " ++ symbol ++ " en el portafolio"
                    return Nothing
                Just asset -> do
                    putStrLn $ "Cantidad de " ++ symbol ++ ": " ++ show (assetCantidad asset)
                    return $ Just (round $ assetCantidad asset)