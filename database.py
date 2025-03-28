import sqlite3
from typing import Dict, List

class Database:
    def __init__(self, db_file: str = "tickets.db"):
        self.db_file = db_file
        self.init_db()

    def init_db(self):
        with sqlite3.connect(self.db_file) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS tracked_tickets (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    ticket TEXT NOT NULL,
                    pais TEXT NOT NULL,
                    estado TEXT NOT NULL,
                    esperando_precio REAL,
                    precio_compra_1 REAL,
                    precio_compra_2 REAL,
                    ratio REAL,
                    precio_venta_1 REAL,
                    precio_venta_2 REAL
                )
            ''')
            conn.commit()

    def get_all_tickets(self) -> List[Dict]:
        with sqlite3.connect(self.db_file) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.cursor()
            cursor.execute('SELECT * FROM tracked_tickets')
            rows = cursor.fetchall()
            return [dict(row) for row in rows]

    def update_ticket_state(self, ticket: str, new_state: str):
        with sqlite3.connect(self.db_file) as conn:
            cursor = conn.cursor()
            cursor.execute(
                'UPDATE tracked_tickets SET estado = ? WHERE ticket = ?',
                (new_state, ticket)
            )
            conn.commit()

    def add_ticket(self, ticket_data: Dict):
        with sqlite3.connect(self.db_file) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                INSERT INTO tracked_tickets (
                    ticket, pais, estado, esperando_precio,
                    precio_compra_1, precio_compra_2, ratio,
                    precio_venta_1, precio_venta_2
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                ticket_data['ticket'],
                ticket_data['pais'],
                ticket_data['estado'],
                ticket_data['esperando_precio'],
                ticket_data['precio_compra_1'],
                ticket_data['precio_compra_2'],
                ticket_data['ratio'],
                ticket_data['precio_venta_1'],
                ticket_data['precio_venta_2']
            ))
            conn.commit()

    def delete_ticket(self, ticket: str) -> bool:
        """
        Elimina un ticket de la base de datos.
        
        Args:
            ticket (str): El símbolo del ticket a eliminar (ej: 'AAPL')
            
        Returns:
            bool: True si se eliminó correctamente, False si el ticket no existía
        """
        with sqlite3.connect(self.db_file) as conn:
            cursor = conn.cursor()
            cursor.execute('DELETE FROM tracked_tickets WHERE ticket = ?', (ticket,))
            deleted = cursor.rowcount > 0
            conn.commit()
            return deleted
