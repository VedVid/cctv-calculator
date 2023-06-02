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
        Fetches the data related to the image size and quality from a database
        and stores it in the dictionary that is later returned.

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
            Dictionary with data fetched from dictionary. Includes also some data passed by the GUI.
            May be empty if fetching data failed.
        """
        d = {}
        resolution_id = self._find_resolution_id(resolution)
        d["frame_size"] = self._find_resolution_frame_size(resolution_id)
        d["compression_factor"] = self._find_compression_factor(compression)
        d["quality_factor"] = self._find_quality_factor(quality)
        d["fps"] = fps
        d["no_of_cameras"] = no_of_cameras
        return d

    def read_transmitters_data(
        self,
        manufacturer: str,
        model: str,
        no_of_cameras: int,
        total_bitrate: int,
        no_of_transmitters: int,
        no_of_receivers: int,
        max_distance: int,
        average_distance: int,
    ) -> dict:
        """
        Fetches the data related to the transmitters from a database,
        and stores it in the dictionary that is later returned.
        Said dictionary includes also data passed from the GUI.

        Parameters
        ----------
        manufacturer : str
            The brand of the transmitter.
        model : str
            The exact model of the transmitter.
        no_of_cameras : int
            Number of cameras in the installation.
        total_bitrate : int
            Total bitrate to be sent by transmitters.
        no_of_transmitters : int
            Amount of transmitters in the installation.
        no_of_receivers : int
            Amount of receivers in the installation
        max_distance : int
            The longest distance (in meters) between transmitter and receiver.
        average_distance : int
            Average distance (in meters) between transmitter and receiver.

        Returns
        -------
        d : dict
            Dictionary with data fetched from dictionary. Includes also some data passed by the GUI.
            May be empty if fetching data failed.
        """
        d = self._find_transmitter_data(manufacturer, model)
        d["no_of_cameras"] = no_of_cameras
        d["total_bitrate"] = total_bitrate
        d["no_of_transmitters"] = no_of_transmitters
        d["no_of_receivers"] = no_of_receivers
        d["max_distance"] = max_distance
        d["average_distance"] = average_distance
        return d

    def _find_transmitter_data(self, manufacturer: str, model: str) -> dict:
        d = {}
        self.connector.cur.execute(
            "SELECT range_max, value_max, range_min, value_min, cameras, max_tx_to_rx, channels_all, channels_ce FROM transmitters WHERE manufacturer LIKE ? AND model LIKE ?",
            (manufacturer, model),
        )
        data = self.connector.cur.fetchone()
        d["max_distance_between_tx_and_rx"] = data[0]
        d["max_bitrate"] = data[1]
        d["min_tested_distance"] = data[2]
        d["min_bitrate"] = data[3]
        d["max_cameras_for_single_transmitter"] = data[4]
        d["max_tx_to_rx"] = data[5]
        d["no_of_channels"] = data[6]
        d["no_of_ce_channels"] = data[7]
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
