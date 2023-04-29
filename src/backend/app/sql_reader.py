# -*- coding: utf-8 -*-


from .sql_connector import SqlConnector


class SqlReader:
    """
    Instance of this class creates its own connector and may fetch data from the database.

    Parameters
    ----------
    sql_connector : SqlConnector
        Instance of SqlConnector that may take a db_name string argument to override the default database name
        specified in `constants.py` file.

    Attributes
    ----------
    connector : SqlConnector
        Instance of SqlConnector, as described in `Parameters`.
    """

    def __init__(self, sql_connector: SqlConnector) -> None:
        self.connector = sql_connector

    def read_image_data(
        self,
        resolution: str,
        compression: str,
        quality: str,
        fps: int,
        no_of_cameras: int,
    ) -> dict:
        """
        Fetches the data from a database and stores it in the dictionary that is later returned.

        Parameters
        ----------
        resolution : str
            Name of the camera resolution.
        compression : str
            Name of the compression method.
        quality : str
            Quality of image set in the cameras.
        fps : int
            Number of frames per second recorded by the cameras.
        no_of_cameras : int
            Number of cameras working in place.

        Returns
        -------
        d : dict
            Dictionary with data fetched from dictionary. May be empty if fetching data failed.
        """
        d = {}
        resolution_id = self._find_resolution_id(resolution)
        d["frame_size"] = self._find_resolution_frame_size(resolution_id)
        d["compression_factor"] = self._find_compression_factor(compression)
        d["quality_factor"] = self._find_quality_factor(quality)
        d["fps"] = fps
        d["no_of_cameras"] = no_of_cameras
        return d

    def _find_resolution_id(self, resolution_name: str) -> int:
        """
        Fetches resolution's id using resolution name.

        Parameters
        ----------
        resolution_name : str
            Name of resolution used in camera, such as 'QCIF' or '1 Mpix'.

        Returns
        -------
        id : float
            Fetched from a database id of resolution.
        """
        self.connector.cur.execute(
            "SELECT id FROM resolution WHERE name LIKE ?", (resolution_name,)
        )
        return self.connector.cur.fetchone()[0]

    def _find_resolution_frame_size(self, resolution_id: int) -> float:
        """
        Fetches the standard frame size of revolution.

        Parameters
        ----------
        resolution_id : int
            id of the resolution.

        Returns
        -------
        framesize : float
            The typical size of a single frame in a specified resolution.
        """
        self.connector.cur.execute(
            "SELECT size FROM framesize WHERE resolution_id = ?", (resolution_id,)
        )
        return self.connector.cur.fetchone()[0]

    def _find_compression_factor(self, compression_name: str) -> float:
        """
        Fetches compression factor using compression name.

        Parameters
        ----------
        compression_name : str
            Name of the compression used, such as 'MPEG-2' or 'h.264'.

        Returns
        -------
        compression_factor : float
            Factor influencing the final frame size.
        """
        self.connector.cur.execute(
            "SELECT factor FROM compression WHERE name LIKE ?", (compression_name,)
        )
        return self.connector.cur.fetchone()[0]

    def _find_quality_factor(self, quality: str) -> float:
        """
        Fetches quality factor using quality name.

        Parameters
        ----------
        quality : str
            'Name' of the quality, as specified in database (either 'low', 'medium' or 'high').

        Returns
        -------
        quality_factor : float
            Factor influencing the final frame size.
        """
        self.connector.cur.execute(
            "SELECT factor FROM quality WHERE name LIKE ?", (quality,)
        )
        return self.connector.cur.fetchone()[0]
