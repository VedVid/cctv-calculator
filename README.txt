What is a CCTV Calculator?
--------------------------
It is a simple application that allows you to quickly calculate the bandwidth of cameras in a CCTV installation and detect the most critical problems in the installation, such as too large distances between radio devices.
Please note that this app is no substitute for accurate designs and common sense. Always check the device specifications, design, and installation location yourself.

How to use a CCTV Calculator?
-----------------------------
The program is equipped with a graphical interface "cctv_calculator". 
You can also use the bandwidth calculator and transmitters validator via the command line. The application will behave differently depending on the passed arguments.
To calculate bandwidth, you must provide: camera resolution, compression method, set image quality, number of recorded frames per second, and the number of cameras in the installation.
You call the program like this:
    calcback --resolution D1 --compression MJPEG --fps 20 --cameras 3
Please note that the arguments with space inside must be inside the quotes, e.g. --resolution "1 Mpix".
To validate the radio installation, you must provide: amount of cameras, total bitrate, amount of transmitters and receivers, maximal and average distance between paired transmitters and receivers, manufacturer and model of radio device.
You call the program like this:
    calcback --cameras 3 --bitrate 30 --transmitters 3 --receivers 1 --distancemax 1000 --distanceaverage 300 --manufacturer "CAMSAT" --model "CDS-6IP 3PoE"
For more info, you can call the program with --help flag.

Will there be a Linux release?
------------------------------
Yes, a Linux release will be available in the near future. At the moment, you can compile the program yourself - the source code is available at https://github.com/VedVid/cctv-calculator

Will there be a Mac release?
----------------------------
No, a Mac release is not planned. I don't have a Mac and maintaining a Mac-specific version is out of my reach. The source code is available on https://github.com/VedVid/cctv-calculator so you can try to compile the code yourself.
