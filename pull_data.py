import requests
from dotenv import load_dotenv
import os

load_dotenv()

bearer = None
refresh = None

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

def call_api(api_url):
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
        update_token()
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
    print(f"Titulos valorizados a peso: {rsp['cuentas'][0]['titulosValorizados']}, total: {rsp['cuentas'][0]['total']}")

def get_portafolio():
    api_url = "https://api.invertironline.com/api/v2/portafolio/argentina"
    rsp = call_api(api_url)
    print(rsp)

get_token()
get_estado_cuenta()
get_portafolio()