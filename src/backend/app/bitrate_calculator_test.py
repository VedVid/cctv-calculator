# -*- coding: utf-8 -*-


from math import isclose

from app.bitrate_calculator import calculate


def test_bitrate_calculator_1():
    # Lowest grade possible.
    # -r QCIF -c MJPEG -q Low -f 10 -m 1
    d = {
        "frame_size": 0.4,
        "compression_factor": 1.8,
        "quality_factor": 0.5,
        "fps": 10,
        "no_of_cameras": 1,
    }
    r = calculate(**d)
    assert isclose(r["total"], 3.60, abs_tol=0.01)
    assert isclose(r["per_camera"], 3.60, abs_tol=0.01)


def test_bitrate_calculator_2():
    # More standard scenario.
    # -r "1 Mpix" -c h.264 -q Medium -f 15 -m 5
    d = {
        "frame_size": 9.3,
        "compression_factor": 1,
        "quality_factor": 1,
        "fps": 15,
        "no_of_cameras": 5,
    }
    r = calculate(**d)
    assert isclose(r["total"], 697.5, abs_tol=0.01)
    assert isclose(r["per_camera"], 139.5, abs_tol=0.01)


def test_bitrate_calculator_3():
    # High-end scnario.
    # -r "16 Mpix" -c h.265+ -q High -f 30 -m 3
    d = {
        "frame_size": 165,
        "compression_factor": 0.2,
        "quality_factor": 1.5,
        "fps": 30,
        "no_of_cameras": 3,
    }
    r = calculate(**d)
    assert isclose(r["total"], 4455, abs_tol=0.1)
    assert isclose(r["per_camera"], 1485, abs_tol=0.01)
