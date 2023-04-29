# -*- coding: utf-8 -*-


import argparse
import sqlite3

from app import bitrate_calculator
from app.csv_writer import csv_bitrate_writer
from app.sql_connector import SqlConnector
from app.sql_reader import SqlReader


if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("-r", "--resolution", type=str)
    argparser.add_argument("-c", "--compression", type=str)
    argparser.add_argument("-q", "--quality", type=str)
    argparser.add_argument("-f", "--fps", type=int)
    argparser.add_argument("-m", "--cameras", type=int)
    args = argparser.parse_args()
    sql_reader = SqlReader(SqlConnector())
    sql_reader.connector.open()
    print(sql_reader.connector.con)
    print(sql_reader.connector.cur)
    try:
        data = sql_reader.read_image_data(args.resolution, args.compression, args.quality, args.fps, args.cameras)
    except sqlite3.OperationalError as e:
        print(f"Fetching data unsuccessful, operation aborted.\n    {e}")
    except TypeError as e:
        print(f"No data fetched, operation aborted.\n    {e}")
    else:
        print(data)
        result = bitrate_calculator.calculate(**data)
        print(result)
        csv_bitrate_writer(result["per_camera"], result["total"])
    finally:
        sql_reader.connector.close()
