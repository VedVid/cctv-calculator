# -*- coding: utf-8 -*-


import argparse
import sqlite3

from app import bitrate_calculator
from app import transmission_validator
from app.csv_writer import csv_transmitters_writer, csv_bitrate_writer
from app.sql_connector import SqlConnector
from app.sql_reader import SqlReader


if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("-r", "--resolution", type=str)
    argparser.add_argument("-c", "--compression", type=str)
    argparser.add_argument("-q", "--quality", type=str)
    argparser.add_argument("-f", "--fps", type=int)
    argparser.add_argument("-m", "--cameras", type=int)
    argparser.add_argument("-b", "--bitrate", type=int)
    argparser.add_argument("-t", "--transmitters", type=int)
    argparser.add_argument("-e", "--receivers", type=int)
    argparser.add_argument("-d", "--distancemax", type=int)
    argparser.add_argument("-i", "--distanceaverage", type=int)
    argparser.add_argument("-a", "--manufacturer", type=str)
    argparser.add_argument("-o", "--model", type=str)
    args = argparser.parse_args()
    sql_reader = SqlReader(SqlConnector())
    sql_reader.connector.open()
    print(sql_reader.connector.con)
    print(sql_reader.connector.cur)
    bandwidth_caculator_args = (
        args.resolution,
        args.compression,
        args.quality,
        args.fps,
        args.cameras,
    )
    transmitters_calculator_args = (
        args.manufacturer,
        args.model,
        args.cameras,
        args.bitrate,
        args.transmitters,
        args.receivers,
        args.distancemax,
        args.distanceaverage,
    )
    if all(bandwidth_caculator_args):
        try:
            data = sql_reader.read_image_data(*bandwidth_caculator_args)
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
    elif all(transmitters_calculator_args):
        try:
            data = sql_reader.read_transmitters_data(*transmitters_calculator_args)
        except sqlite3.OperationalError as e:
            print(f"Fetching data unsuccessful, operation aborted.\n    {e}")
        except TypeError as e:
            print(f"No data fetched, operation aborted.\n    {e}")
        else:
            validation = transmission_validator.validate(**data)
            import pprint

            pprint.pprint(validation)
            csv_transmitters_writer(validation)
        finally:
            sql_reader.connector.close()
    else:
        print("ERROR!")
