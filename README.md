# IOL Bot

Un bot de trading automatizado para InvertirOnline (IOL) escrito en Haskell.

## Características

- Autenticación automática con la API de IOL
- Sistema de trading basado en tickets con múltiples estados
- Monitoreo de precios en tiempo real
- Ejecución automática de órdenes de compra y venta
- Seguimiento del dólar MEP para cada símbolo
- Base de datos SQLite para persistencia de datos
- Comparación del dólar MEP de cada símbolo con AL30 como referencia
- Registro de órdenes ejecutadas en archivo de log
- Obtención eficiente de cotizaciones usando la API de cotizaciones masivas

## Configuración

1. Crear un archivo `.env` en el directorio raíz con las siguientes variables:
```
IOL_USERNAME=tu_usuario
IOL_PASSWORD=tu_contraseña
```

2. Configurar los tickets en `Main.hs` con los siguientes parámetros:
- Nombre del ticket
- Estado inicial (normalmente Waiting)
- Precios objetivo para:
  - Primera compra
  - Segunda compra
  - Primera venta
  - Segunda venta
  - Take profit
  - Stop loss

## Estrategia de Trading

El bot implementa una estrategia basada en:

1. Comparación del dólar MEP:
   - Para compras: El dólar MEP del símbolo debe ser > 90% del dólar MEP de AL30
   - Para ventas: El dólar MEP del símbolo debe ser < 110% del dólar MEP de AL30

2. Estados del ticket:
   - Waiting: Esperando primera oportunidad de compra
   - FirstBuy: Primera compra realizada, esperando segunda compra o primera venta
   - SecondBuy: Segunda compra realizada, esperando primera venta
   - FirstSell: Primera venta realizada, esperando segunda venta
   - SecondSell: Segunda venta realizada
   - StopLoss: Stop loss alcanzado
   - TakeProfit: Take profit alcanzado

3. Logging y Monitoreo:
   - Todas las órdenes ejecutadas se registran en `ordenes_ejecutadas.log`
   - El sistema utiliza la API de cotizaciones masivas para obtener datos de manera eficiente
   - Se monitorean las puntas de compra/venta, volumen y cantidad de operaciones

## Base de Datos

El bot utiliza SQLite para almacenar:
- Tickets y sus estados
- Tenencias actuales
- Estado de cuenta
- Tokens de autenticación

## Requisitos

- GHC (Glasgow Haskell Compiler)
- Cabal
- SQLite3

sudo apt install ghc cabal-install sqlite3 libsqlite3-dev


## Instalación

1. Clonar el repositorio
2. Instalar dependencias:
```bash
cabal update
cabal install
```

3. Compilar:
```bash
cabal build
```

4. Ejecutar:
```bash
cabal run
```

## Estructura del Proyecto

- `src/`
  - `Api.hs`: Funciones para interactuar con la API de IOL
  - `Database.hs`: Manejo de la base de datos SQLite
  - `Trading.hs`: Lógica de trading y procesamiento de tickets
  - `Types.hs`: Definición de tipos de datos
  - `Utils.hs`: Funciones de utilidad
- `app/`
  - `Main.hs`: Punto de entrada y configuración inicial
- `logs/`
  - `ordenes_ejecutadas.log`: Registro de todas las órdenes ejecutadas

## Seguridad

- Las credenciales se manejan a través de variables de entorno
- Los tokens se almacenan de forma segura en la base de datos
- Las operaciones requieren confirmación basada en múltiples condiciones
