# -*- coding: utf-8 -*-


import csv


def csv_bitrate_writer(per_camera: float, total: float) -> None:
    """
    Writes data fetched by SqlReader to the csv file that will be later parsed by GUI application.

    Parameters
    ----------
    per_camera : float
        Bitrate per a single camera in an installation.
    total : float
        Bitrate of the whole installation.
    """

    with open("cameras_bandwidth.csv", mode="w") as f:
        f_writer = csv.writer(
            f, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        f_writer.writerow(["%.2f" % per_camera, "%.2f" % total])


def csv_transmitters_writer(comments: list) -> None:
    with open("transmitters_validation.csv", mode="w") as f:
        f_writer = csv.writer(
            f, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        for comment in comments:
            f_writer.writerow([comment])


def csv_error_writer(txt: str, filename: str) -> None:
    with open(filename, mode="w") as f:
        f_writer = csv.writer(f)
        f_writer.writerow([txt])
