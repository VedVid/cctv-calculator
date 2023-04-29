# -*- coding: utf-8 -*-


import sqlite3

from app import bitrate_calculator
from app.sql_connector import SqlConnector
from app.sql_reader import SqlReader


if __name__ == "__main__":
    sql_reader = SqlReader(SqlConnector())
    sql_reader.connector.open()
    print(sql_reader.connector.con)
    print(sql_reader.connector.cur)
    try:
        data = sql_reader.read_image_data("1 Mpix", "h.264", "Low", 30, 5)
    except sqlite3.OperationalError as e:
        print(f"Fetching data unsuccessful, operation aborted.\n    {e}")
    else:
        print(data)
        result = bitrate_calculator.calculate(**data)
        print(result)
    finally:
        sql_reader.connector.close()
