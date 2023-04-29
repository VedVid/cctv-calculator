# -*- coding: utf-8 -*-


def calculate(**data_dict):
    """
    Calculating the bandwidth of the cameras.

    Parameters
    ----------
    data_dict : kwargs, dict
        Dictionary filled with data fetched by SqlReader. It uses **kwargs instead of passing the dict directly,
        due to the unpredictability of dicts passed directly.

    Returns
    -------
    bandwidth : dict
        Dictionary with two keys: 'total' storing the total bitrate, and 'per_camera' storing bandwidth per camera.
    """
    bandwidth = {}
    total = 1
    for arg in data_dict.values():
        total *= arg
    bandwidth["total"] = total
    bandwidth["per_camera"] = total / data_dict["no_of_cameras"]
    return bandwidth
