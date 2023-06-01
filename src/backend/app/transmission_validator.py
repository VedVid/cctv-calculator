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
            f"The number of cameras ({no_of_cameras}) exceeds the number of available channels ({data_dict['no_of_channels']})."
        )
    if no_of_cameras > data_dict["no_of_ce_channels"]:
        comments.append(
            f"The number of cameras ({no_of_cameras}) exceeds the number of available CE channels ({data_dict['no_of_ce_channels']})."
        )
        if data_dict["no_of_channels"] > data_dict["no_of_ce_channels"]:
            no_of_non_ce_channels = (
                data_dict["no_of_channels"] - data_dict["no_of_ce_channels"]
            )
            comments.append(
                f"There are available non-CE channels: {no_of_non_ce_channels}."
            )
            comments.append(
                "Check which channels you can legally use at the installation site."
            )
    if no_of_cameras > max_cameras_in_installation_ports:
        comments.append(
            f"The number of cameras ({no_of_cameras}) exceeds the number of available ports ({max_cameras_in_installation_ports})."
        )

    # Check if there is enough receivers for the transmitters on the site.
    if (
        data_dict["no_of_transmitters"]
        > data_dict["no_of_receivers"] * data_dict["max_tx_to_rx"]
    ):
        comments.append(
            f"There is not enough receivers ({data_dict['no_of_receivers']}) to connect all transmitters ({data_dict['no_of_transmitters']}, max {data_dict['max_tx_to_rx']} tx to 1 rx)."
        )

    # Check if the distance is not too great for tx+rx to work well.
    if data_dict["max_distance"] > data_dict["max_distance_between_tx_and_rx"]:
        comments.append(
            f"The distance between the furthest points ({data_dict['max_distance']} m) is too large (max {data_dict['max_distance_between_tx_and_rx']} m)"
        )

    # Check the distances and bitrate.
    average_available_bitrate = (
        data_dict["max_bitrate"] - data_dict["min_bitrate"]
    ) / (
        (data_dict["max_distance_between_tx_and_rx"] - data_dict["min_tested_distance"])
        / 100
    )
    total_available_bitrate = data_dict["max_bitrate"] - (
        (data_dict["average_distance"] / 100) * average_available_bitrate
    )
    if total_available_bitrate < data_dict["total_bitrate"]:
        comments.append(
            f"Total bitrate of cameras ({data_dict['total_bitrate']} Mbps) exceeds bitrate provided by receivers ({int(total_available_bitrate)} Mbps)."
        )

    return comments
