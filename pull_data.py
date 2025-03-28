import requests
from dotenv import load_dotenv
import os
import time
from database import Database
from datetime import datetime

load_dotenv()

bearer = None
refresh = None
dinero_disponible = 0
db = Database()

def get_token():
    global bearer, refresh
    url = "https://api.invertironline.com/token"
    headers = {
        "Content-Type": "application/x-www-form-urlencoded"
    }
    data = {
        "username": os.getenv("IOL_USERNAME"),
        "password": os.getenv("IOL_PASSWORD"),
        "grant_type": "password"
    }

    response = requests.post(url, headers=headers, data=data)

    if response.status_code == 200:
        print("Access Token granted")
    else:
        print("Error:", response.status_code, response.text)
        assert(False)

    bearer = response.json()["access_token"]
    refresh = response.json()["refresh_token"]
    return

def update_token():
    global bearer, refresh
    url = "https://api.invertironline.com/token"
    headers = {
        "Content-Type": "application/x-www-form-urlencoded"
    }
    data = {
        "refresh_token": refresh,
        "grant_type": "refresh_token"
    }

    response = requests.post(url, headers=headers, data=data)

    if response.status_code == 200:
        print("Access Token granted")
    else:
        print("Error:", response.status_code, response.text)
        assert(False)

    bearer = response.json()["access_token"]
    refresh = response.json()["refresh_token"]

def call_api(api_url, data=None):
    global bearer
    headers = {
        "Authorization": f"Bearer {bearer}",
        "Content-Type": "application/json"
    }

    response = requests.get(api_url, headers=headers)

    if response.status_code == 200:
        print("API call successful")
    else:
        print("Renovando token...")
        get_token()
        headers["Authorization"] = f"Bearer {bearer}"
        response = requests.get(api_url, headers=headers)

        if response.status_code != 200:
            print("Error al renovar el token:", response.status_code, response.text)
            assert(False)

    return response.json()

def get_estado_cuenta():
    api_url = "https://api.invertironline.com/api/v2/estadocuenta"
    rsp = call_api(api_url)
    saldos_arg = rsp['cuentas'][0]['saldos']
    print(f"Pesos disponibles: ahora: {saldos_arg[0]['disponible']} en 24hs: {saldos_arg[1]['disponible']} en 48hs: {saldos_arg[2]['disponible']}, comprometido: {saldos_arg[0]['comprometido']}")
    saldos_dol = rsp['cuentas'][1]['saldos']
    print(f"Dolares disponibles: ahora: {saldos_dol[0]['disponible']} en 24hs: {saldos_dol[1]['disponible']} en 48hs: {saldos_dol[2]['disponible']}, comprometido: {saldos_dol[0]['comprometido']}")
    print(f"Titulos valorizados a peso: {format(rsp['cuentas'][0]['titulosValorizados'], ',.2f')}, total: {format(rsp['cuentas'][0]['total'], ',.2f')}")

def get_portafolio():
    api_url = "https://api.invertironline.com/api/v2/portafolio/argentina"
    rsp = call_api(api_url, data={"pais": "argentina"})
    print(rsp)

def get_mep():
    api_url = "https://api.invertironline.com/api/v2/Cotizaciones/MEP/AL30"
    rsp = call_api(api_url, data={})
    print(rsp)

def get_cotizacion(simbolo):
    api_url = f"https://api.invertironline.com/api/v2/bCBA/Titulos/{simbolo}/CotizacionDetalle"
    rsp = call_api(api_url, data={})
    print(rsp['ultimoPrecio'], rsp["puntas"][0]["precioVenta"]-rsp["puntas"][0]["precioCompra"])

def comprar(simbolo, cantidad, precio):
    api_url = "https://api.invertironline.com/api/v2/Comprar"
    # ver de elejir el mejor plazo
    hoy = datetime.now()
    expira = hoy + timedelta(days=29)

    data = {"mercado": "bCBA", "simbolo": simbolo, "cantidad": cantidad, "precio": precio, "plazo": "t1",
            "validez": f"{expira.year}-{expira.month}-{expira.day}:00:00.000Z", "tipoOrden": "precioLimite"}#, "monto": cantidad*precio }
    rsp = call_api(api_url, data=data)
    print(rsp)
    return rsp

def vender(simbolo, cantidad, precio):
    api_url = "https://api.invertironline.com/api/v2/Vender"
    # ver de elejir el mejor plazo
    hoy = datetime.now()
    expira = hoy + timedelta(days=29)

    data = {"mercado": "bCBA", "simbolo": simbolo, "cantidad": cantidad, "precio": precio, "plazo": "t1",
            "validez": f"{expira.year}-{expira.month}-{expira.day}:00:00.000Z", "tipoOrden": "precioLimite"}#, "monto": cantidad*precio }
    rsp = call_api(api_url, data=data)
    print(rsp)
    return rsp

get_token()
get_estado_cuenta()
get_portafolio()
get_mep()

configuraciones = {"porcentaje_libre": 0.2, "stop_loss": 0.5}

while True:
    tracked_tickets = db.get_all_tickets()
    for ticket in tracked_tickets:
        precio, spread = get_cotizacion(ticket["ticket"])
        if ticket["estado"] == "afuera":
            if precio < ticket["precio_compra_1"]:
                db.update_ticket_state(ticket["ticket"], "adentro_1")  # Comprar
                comprar(ticket["ticket"], 1, precio)
        elif ticket["estado"] == "adentro_1":
            if precio > ticket["precio_compra_2"]:
                db.update_ticket_state(ticket["ticket"], "adentro_2")  # Comprar
                comprar(ticket["ticket"], 1, precio)
        
        if ticket["estado"] == "adentro_2" or ticket["estado"] == "adentro_1":
            if precio > ticket["precio_venta_1"]:
                db.update_ticket_state(ticket["ticket"], "afuera")  # Vender
                vender(ticket["ticket"], 1, precio)
            elif precio < ticket["precio_venta_2"]:
                db.update_ticket_state(ticket["ticket"], "adentro_1")  # Vender
                vender(ticket["ticket"], 1, precio)

    time.sleep(10)

# Inicializar la base de datos con los tickets iniciales
initial_tickets = [
    {
        "ticket": "AAPL",
        "pais": "argentina",
        "estado": "afuera",
        "esperando_precio": 2,
        "precio_compra_1": 12140,
        "precio_compra_2": 10725,
        "ratio": 0.05,
        "precio_venta_1": 15275,
        "precio_venta_2": 16055
    }
]

for ticket in initial_tickets:
    db.add_ticket(ticket)

# To-Do
# 1. Que se fije en la punta correspondiente, no en el ultimo precio
# 2. mejor plazo
# 3. aplicar precio mercado trailing
