# -*- coding: utf-8 -*-


import argparse
import logging
import sqlite3

from app import bitrate_calculator
from app import constants
from app import transmission_validator
from app.csv_writer import csv_transmitters_writer, csv_bitrate_writer
from app.sql_connector import SqlConnector
from app.sql_reader import SqlReader


if __name__ == "__main__":
    logging.basicConfig(filename="cctv_calculator.log",
                        filemode='w',
                        format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                        datefmt='%H:%M:%S',
                        level=logging.INFO)
    logging.info("Running cctv calculator cli app.")
    logging.info("Parsing arguments...")
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
    logging.info("    DONE.")
    logging.info("The parameters passed:")
    logging.info(f"    {args}.")
    logging.info("Connecting to the database...")
    sql_reader = SqlReader(SqlConnector())
    sql_reader.connector.open()
    logging.info("    DONE.")
    if constants.PRINT_DEBUG:
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
            logging.info("Fetching image data...")
            data = sql_reader.read_image_data(*bandwidth_caculator_args)
        except sqlite3.OperationalError as e:
            logging.critical(f"Fetching data unsuccessful, operation aborted.\n    {e}")
        except TypeError as e:
            logging.critical(f"No data fetched, operation aborted.\n    {e}")
        else:
            logging.info("    DONE.")
            logging.info("Calculating...")
            result = bitrate_calculator.calculate(**data)
            logging.info("    DONE.")
            logging.info("Writing data to csv file...")
            csv_bitrate_writer(result["per_camera"], result["total"])
            logging.info("    DONE.")
        finally:
            logging.info("Closing SQL connection...")
            sql_reader.connector.close()
            logging.info("    DONE.")
    elif all(transmitters_calculator_args):
        try:
            logging.info("Fetching transmitters data...")
            data = sql_reader.read_transmitters_data(*transmitters_calculator_args)
        except sqlite3.OperationalError as e:
            logging.critical(f"Fetching data unsuccessful, operation aborted.\n    {e}")
        except TypeError as e:
            logging.critical(f"No data fetched, operation aborted.\n    {e}")
        else:
            logging.info("    DONE.")
            logging.info("Validating...")
            validation = transmission_validator.validate(**data)
            logging.info("    DONE.")
            logging.info("Writing data to csv file...")
            csv_transmitters_writer(validation)
            logging.info("    DONE.")
        finally:
            logging.info("Closing SQL connection...")
            sql_reader.connector.close()
            logging.info("    DONE.")
    else:
        logging.critical("Arguments passed to the application do not match any available schema.")
