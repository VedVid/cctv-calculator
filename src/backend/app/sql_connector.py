# -*- coding: utf-8 -*-


import os.path
import sqlite3

from .constants import DATABASE_NAME


class SqlConnector:
    """
    Instance of this class connects to the sqlite database and enables the cursor.

    Parameters
    ----------
    db_name : str
        Name of the databse. If not provided, uses name specified in constants file.

    Attributes
    ----------
    db_name : str
        Name of the database, as passed on creating new instance. Uses name specified in constants file by default.
        FileNotFoundError is raised, if the database file is not found.
    opened : bool
        Tracks wether database connection is opened or closed. Necessary because there is no method like `isclosed()`
        for sqlite3.Connection class.
    con : sqlite3.Connection
        Connection to the sqlite3 database.
    cur : sqlite3.Cursor
        Cursor in the sqlite3 database to which a class instance is connected to.
    """

    def __init__(self, db_name: str = DATABASE_NAME) -> None:
        if os.path.exists(db_name):
            self.db_name = db_name
        else:
            print(f"Database file {db_name} not found!")
            raise FileNotFoundError
        self.opened = False
        self.con = None
        self.cur = None

    def open(self) -> None:
        """
        Opens the connection to the database and running the cursor. It sets `opened` attribute to True
        to make connection state easier to track.
        """
        self.con = sqlite3.connect(self.db_name)
        self.cur = self.con.cursor()
        self.opened = True

    def close(self) -> None:
        """
        Closes the connection to the database, and resetting `con`nection and `cur`sor attributes to None.
        It also sets `opened` attribute to False to make connection state easier to track.
        """
        self.con.close()
        self.con = None
        self.cur = None
        self.opened = False
