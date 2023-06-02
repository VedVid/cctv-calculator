# -*- coding: utf-8 -*-


from app.transmission_validator import validate


class TestTransmissionValidation:
    transmitter_data_1 = {
        "manufacturer": "Camsat",
        "model": "CDS-6IP 3PoE",
        "max_distance_between_tx_and_rx": 2000,
        "max_bitrate": 63,
        "min_tested_distance": 100,
        "min_bitrate": 12,
        "max_cameras_for_single_transmitter": 3,
        "max_tx_to_rx": 8,
        "no_of_channels": 16,
        "no_of_ce_channels": 16,
    }

    transmitter_data_2 = {
        "manufacturer": "Camsat",
        "model": "CAM-Analog 2.0",
        "max_distance_between_tx_and_rx": 500,
        "max_bitrate": 2.5,
        "min_tested_distance": 0,
        "min_bitrate": 2,
        "max_cameras_for_single_transmitter": 1,
        "max_tx_to_rx": 1,
        "no_of_channels": 8,
        "no_of_ce_channels": 2,
    }

    # valid for transmitter_data_1
    installation_data_1 = {
        "no_of_cameras": 9,
        "total_bitrate": 15,
        "no_of_transmitters": 3,
        "no_of_receivers": 2,
        "max_distance": 2000,
        "average_distance": 500,
    }

    # partially valid for transmitter_data_1
    installation_data_2 = {
        "no_of_cameras": 9,
        "total_bitrate": 170,
        "no_of_transmitters": 3,
        "no_of_receivers": 2,
        "max_distance": 2000,
        "average_distance": 1400,
    }

    # totally invalid for transmitter_data_1
    installation_data_3 = {
        "no_of_cameras": 45,
        "total_bitrate": 900,
        "no_of_transmitters": 10,
        "no_of_receivers": 1,
        "max_distance": 3000,
        "average_distance": 2100,
    }

    # valid for transmitter_data_2
    installation_data_4 = {
        "no_of_cameras": 1,
        "total_bitrate": 2,
        "no_of_transmitters": 1,
        "no_of_receivers": 1,
        "max_distance": 300,
        "average_distance": 300,
    }

    # partially valid for transmitter_data_2
    installation_data_5 = {
        "no_of_cameras": 2,
        "total_bitrate": 2,
        "no_of_transmitters": 3,
        "no_of_receivers": 2,
        "max_distance": 600,
        "average_distance": 400,
    }

    # totally invalid for transmitter_data_2
    installation_data_6 = {
        "no_of_cameras": 4,
        "total_bitrate": 20,
        "no_of_transmitters": 3,
        "no_of_receivers": 2,
        "max_distance": 1200,
        "average_distance": 800,
    }

    def test_validation_1(self):
        d = {**self.transmitter_data_1, **self.installation_data_1}
        r = validate(**d)
        assert not r

    def test_validation_2(self):
        d = {**self.transmitter_data_1, **self.installation_data_2}
        r = validate(**d)
        assert "Total bitrate of cameras exceeds bitrate provided by receivers." in r
        assert len(r) == 1

    def test_validation_3(self):
        d = {**self.transmitter_data_1, **self.installation_data_3}
        r = validate(**d)
        y = {
            "The number of cameras exceeds the number of available channels.",
            "The number of cameras exceeds the number of available CE channels.",
            "The number of cameras exceeds the number of available ports.",
            "There is not enough receivers to connect all transmitters.",
            "The distance between the furthest points is too large.",
            "Total bitrate of cameras exceeds bitrate provided by receivers.",
        }
        assert y.issubset(set(r)) and y.issuperset(set(r))

    def test_validation_4(self):
        d = {**self.transmitter_data_2, **self.installation_data_4}
        r = validate(**d)
        assert not r

    def test_validation_5(self):
        d = {**self.transmitter_data_2, **self.installation_data_5}
        r = validate(**d)
        assert "There is not enough receivers to connect all transmitters." in r
        assert "The distance between the furthest points is too large." in r
        assert len(r) == 2

    def test_validation_6(self):
        d = {**self.transmitter_data_2, **self.installation_data_6}
        r = validate(**d)
        y = {
            "The number of cameras exceeds the number of available CE channels.",
            "There are available non-CE channels.",
            "Check which channels you can legally use at the installation site.",
            "The number of cameras exceeds the number of available ports.",
            "There is not enough receivers to connect all transmitters.",
            "The distance between the furthest points is too large.",
            "Total bitrate of cameras exceeds bitrate provided by receivers.",
        }
        assert y.issubset(set(r)) and y.issuperset(set(r))
