# IOL Trading Bot

Un bot de trading para InvertirOnline (IOL) que permite gestionar operaciones automáticas basadas en niveles de precios predefinidos.

## Características Implementadas

### Sistema de Autenticación
- Autenticación automática con IOL API
- Manejo seguro de credenciales mediante archivo .env
- Almacenamiento y gestión de tokens de acceso

### Sistema de Trading
- Definición de tickets de trading con:
  - Múltiples niveles de precios (compra1, compra2, venta1, venta2)
  - Stop Loss y Take Profit
  - Estado del ticket (Waiting, FirstBuy, SecondBuy, FirstSell, SecondSell, etc.)
  - Seguimiento de puntas actuales del mercado
  - Timestamp de última actualización

### Base de Datos
- Almacenamiento persistente en SQLite
- Tablas implementadas:
  - `tickets`: Almacena los niveles de trading y estados
  - `estado_cuenta`: Información de la cuenta
  - `tenencias`: Posiciones actuales
  - `tokens`: Gestión de tokens de autenticación

### API Integration
- Conexión con IOL API v2
- Obtención de estado de cuenta
- Consulta de cotizaciones en tiempo real

## Objetivos del Proyecto

1. Automatizar operaciones de trading basadas en niveles de precios predefinidos
2. Mantener un seguimiento preciso de las operaciones y su estado
3. Minimizar el riesgo mediante stop loss automático
4. Maximizar ganancias con take profit y múltiples niveles de entrada/salida

## TO DO

### Corto Plazo
- [ ] Implementar actualización automática de puntas de mercado
- [ ] Agregar sistema de logging para seguimiento de operaciones
- [ ] Implementar lógica de ejecución de órdenes
- [ ] Agregar validaciones de saldo y tenencias antes de operar

### Mediano Plazo
- [ ] Desarrollar interfaz web para monitoreo
- [ ] Implementar sistema de notificaciones (email/telegram)
- [ ] Agregar análisis técnico básico

### Largo Plazo
- [ ] Implementar machine learning para optimización de niveles
- [ ] Agregar soporte para múltiples estrategias
- [ ] Desarrollar sistema de gestión de riesgo avanzado
- [ ] Implementar análisis de correlación entre instrumentos

## Uso

1. Crear archivo `.env` con credenciales:
```
IOL_USERNAME=tu_usuario
IOL_PASSWORD=tu_password
```

2. Compilar y ejecutar:
```bash
cabal run
```

## Estructura del Proyecto

```
IOL-bot/
├── app/
│   └── Main.hs          # Punto de entrada
├── src/
│   ├── Api.hs          # Integración con IOL API
│   ├── Database.hs     # Manejo de base de datos
│   └── Types.hs        # Definiciones de tipos
├── .env                # Credenciales (no incluido en repo)
└── iol.db             # Base de datos SQLite
```

## Contribuir

Las contribuciones son bienvenidas. Por favor, crear un issue primero para discutir los cambios propuestos.
