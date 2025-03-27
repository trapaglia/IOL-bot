import requests
from dotenv import load_dotenv
import os

load_dotenv()

def get_token():
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
    return bearer, refresh

def call_api(bearer, api_url):
    headers = {
        "Authorization": f"Bearer {bearer}",
        "Content-Type": "application/json"
    }

    response = requests.get(api_url, headers=headers)

    if response.status_code == 200:
        print("API call successful")
    else:
        print("Error:", response.status_code, response.text)
        assert(False)

    return response.json()

def get_estado_cuenta(bearer):
    api_url = "https://api.invertironline.com/api/v2/estadocuenta"
    return call_api(bearer, api_url)

bearer, refresh = get_token()
print(get_estado_cuenta(bearer))

pass