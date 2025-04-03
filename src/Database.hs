{-# LANGUAGE OverloadedStrings #-}

module Database
    ( initializeDatabase
    , insertCotizacion
    , insertTenencia
    , insertEstadoCuenta
    , insertToken
    , getLatestCotizacion
    , getLatestTenencias
    , getLatestEstadoCuenta
    , getLatestToken
    , DBCotizacion(..)
    , DBTenencia(..)
    , DBEstadoCuenta(..)
    , DBToken(..)
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Database.SQLite.Simple.ToRow()
import qualified Data.Text as T
import Data.Time
import Types

-- Tipos de datos para la base de datos
data DBCotizacion = DBCotizacion
    { cotizacionSymbol :: T.Text
    , cotizacionUltimoPrecio :: Double
    , cotizacionPrecioCompra :: Double
    , cotizacionPrecioVenta :: Double
    , cotizacionTimestamp :: UTCTime
    } deriving (Show)

data DBTenencia = DBTenencia
    { tenenciaSymbol :: T.Text
    , tenenciaCantidad :: Int
    , tenenciaPrecioCompra :: Double
    , tenenciaTimestamp :: UTCTime
    } deriving (Show)

data DBEstadoCuenta = DBEstadoCuenta
    { cuentaNumero :: T.Text
    , cuentaTipo :: T.Text
    , cuentaMoneda :: T.Text
    , cuentaSaldo :: Double
    , cuentaComprometido :: Double
    , cuentaDisponible :: Double
    , cuentaTimestamp :: UTCTime
    } deriving (Show)

data DBToken = DBToken
    { tokenValue :: T.Text
    , tokenTimestamp :: UTCTime
    } deriving (Show)

instance FromRow DBCotizacion where
    fromRow = DBCotizacion <$> field <*> field <*> field <*> field <*> field

instance ToRow DBCotizacion where
    toRow c = [SQLText $ cotizacionSymbol c, 
               SQLFloat $ cotizacionUltimoPrecio c,
               SQLFloat $ cotizacionPrecioCompra c,
               SQLFloat $ cotizacionPrecioVenta c,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ cotizacionTimestamp c]

instance FromRow DBTenencia where
    fromRow = DBTenencia <$> field <*> field <*> field <*> field

instance ToRow DBTenencia where
    toRow t = [SQLText $ tenenciaSymbol t,
               SQLInteger $ fromIntegral $ tenenciaCantidad t,
               SQLFloat $ tenenciaPrecioCompra t,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ tenenciaTimestamp t]

instance FromRow DBEstadoCuenta where
    fromRow = DBEstadoCuenta <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBEstadoCuenta where
    toRow e = [SQLText $ cuentaNumero e,
               SQLText $ cuentaTipo e,
               SQLText $ cuentaMoneda e,
               SQLFloat $ cuentaSaldo e,
               SQLFloat $ cuentaComprometido e,
               SQLFloat $ cuentaDisponible e,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ cuentaTimestamp e]

instance FromRow DBToken where
    fromRow = DBToken <$> field <*> field

instance ToRow DBToken where
    toRow t = [SQLText $ tokenValue t,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ tokenTimestamp t]

-- Inicialización de la base de datos
initializeDatabase :: IO Connection
initializeDatabase = do
    conn <- open "iol.db"
    -- Eliminar tablas existentes para recrearlas con la nueva estructura
    execute_ conn "DROP TABLE IF EXISTS estado_cuenta"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS cotizaciones (\
        \symbol TEXT NOT NULL,\
        \ultimo_precio REAL NOT NULL,\
        \precio_compra REAL NOT NULL,\
        \precio_venta REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (symbol, timestamp))"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS tenencias (\
        \symbol TEXT NOT NULL,\
        \cantidad INTEGER NOT NULL,\
        \precio_compra REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (symbol, timestamp))"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS estado_cuenta (\
        \numero_cuenta TEXT NOT NULL,\
        \tipo_cuenta TEXT NOT NULL,\
        \moneda TEXT NOT NULL,\
        \saldo REAL NOT NULL,\
        \comprometido REAL NOT NULL,\
        \disponible REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (numero_cuenta, tipo_cuenta, timestamp))"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS tokens (\
        \token TEXT NOT NULL,\
        \timestamp DATETIME NOT NULL PRIMARY KEY)"
    
    return conn

-- Funciones de inserción
insertCotizacion :: Connection -> T.Text -> CotizacionDetalle -> IO ()
insertCotizacion conn symbol cotizacion = do
    now <- getCurrentTime
    case puntas cotizacion of
        (p:_) -> execute conn "INSERT OR REPLACE INTO cotizaciones (symbol, ultimo_precio, precio_compra, precio_venta, timestamp) VALUES (?, ?, ?, ?, datetime(?))"
                    (DBCotizacion symbol (ultimoPrecio cotizacion) (precioCompra p) (precioVenta p) now)
        [] -> return ()

insertTenencia :: Connection -> T.Text -> Int -> Double -> IO ()
insertTenencia conn symbol cantidad precio = do
    now <- getCurrentTime
    execute conn "INSERT OR REPLACE INTO tenencias (symbol, cantidad, precio_compra, timestamp) VALUES (?, ?, ?, datetime(?))"
        (DBTenencia symbol cantidad precio now)

insertEstadoCuenta :: Connection -> Cuenta -> IO ()
insertEstadoCuenta conn cuenta = do
    now <- getCurrentTime
    case saldos cuenta of
        (s:_) -> execute conn "INSERT OR REPLACE INTO estado_cuenta (numero_cuenta, tipo_cuenta, moneda, saldo, comprometido, disponible, timestamp) VALUES (?, ?, ?, ?, ?, ?, datetime(?))"
                    (DBEstadoCuenta 
                        (T.pack $ numero cuenta)
                        (T.pack $ tipo cuenta)
                        (T.pack $ moneda cuenta)
                        (saldo s)
                        (comprometido s)
                        (disponible s)
                        now)
        [] -> return ()

insertToken :: Connection -> String -> IO ()
insertToken conn token = do
    now <- getCurrentTime
    execute conn "INSERT OR REPLACE INTO tokens (token, timestamp) VALUES (?, datetime(?))"
        (DBToken (T.pack token) now)

-- Funciones de consulta
getLatestCotizacion :: Connection -> T.Text -> IO (Maybe DBCotizacion)
getLatestCotizacion conn symbol = do
    results <- query conn
        "SELECT * FROM cotizaciones WHERE symbol = ? ORDER BY timestamp DESC LIMIT 1"
        (Only symbol) :: IO [DBCotizacion]
    return $ case results of
        (x:_) -> Just x
        [] -> Nothing

getLatestTenencias :: Connection -> IO [DBTenencia]
getLatestTenencias conn = do
    query_ conn
        "SELECT DISTINCT t1.* FROM tenencias t1 \
        \INNER JOIN (SELECT symbol, MAX(timestamp) as max_ts FROM tenencias GROUP BY symbol) t2 \
        \ON t1.symbol = t2.symbol AND t1.timestamp = t2.max_ts"

getLatestEstadoCuenta :: Connection -> IO [DBEstadoCuenta]
getLatestEstadoCuenta conn = do
    putStrLn "Ejecutando consulta SQL para estado de cuenta..."
    results <- query_ conn
        "SELECT e1.* FROM estado_cuenta e1 \
        \INNER JOIN (SELECT numero_cuenta, tipo_cuenta, MAX(timestamp) as max_ts FROM estado_cuenta GROUP BY numero_cuenta, tipo_cuenta) e2 \
        \ON e1.numero_cuenta = e2.numero_cuenta AND e1.tipo_cuenta = e2.tipo_cuenta AND e1.timestamp = e2.max_ts \
        \ORDER BY e1.numero_cuenta"
    putStrLn $ "Número de resultados encontrados: " ++ show (length results)
    return results

getLatestToken :: Connection -> IO (Maybe DBToken)
getLatestToken conn = do
    results <- query_ conn
        "SELECT * FROM tokens ORDER BY timestamp DESC LIMIT 1" :: IO [DBToken]
    return $ case results of
        (x:_) -> Just x
        [] -> Nothing
