# -*- coding: utf-8 -*-


def validate(**data_dict):
    comments = []

    # Check if there is too many cameras or too few transmitters.
    max_cameras_in_installation_ports = (
        data_dict["no_of_transmitters"]
        * data_dict["max_cameras_for_single_transmitter"]
    )
    no_of_cameras = data_dict["no_of_cameras"]
    if no_of_cameras > data_dict["no_of_channels"]:
        comments.append(
            "The number of cameras exceeds the number of available channels."
        )
    if no_of_cameras > data_dict["no_of_ce_channels"]:
        comments.append(
            "The number of cameras exceeds the number of available CE channels."
        )
        if data_dict["no_of_channels"] > data_dict["no_of_ce_channels"]:
            comments.append("There are available non-CE channels.")
            comments.append("Check which channels you can legally use at the installation site.")
    if no_of_cameras > max_cameras_in_installation_ports:
        comments.append("The number of cameras exceeds the number of available ports.")

    # Check if there is enough receivers for the transmitters on the site.
    if (
            data_dict["no_of_transmitters"] > data_dict["no_of_receivers"] * data_dict["max_tx_to_rx"]
    ):
        comments.append("There is not enough receivers to connect all transmitters.")

    # Check if the distance is not too great for tx+rx to work well.
    if data_dict["max_distance"] > data_dict["max_distance_between_tx_and_rx"]:
        comments.append("The distance between the furthest points is too large.")

    # Check the distances and bitrate.
    average_available_bitrate = (data_dict["max_bitrate"] - data_dict["min_bitrate"]) / (
        (data_dict["max_distance_between_tx_and_rx"] - data_dict["min_tested_distance"]) / 100
    )
    total_available_bitrate = data_dict["max_bitrate"] - ((data_dict["average_distance"] / 100) * average_available_bitrate)
    if total_available_bitrate < data_dict["total_bitrate"]:
        comments.append(
            "Total bitrate of cameras exceeds bitrate provided by receivers."
        )

    return comments
