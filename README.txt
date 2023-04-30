What is a CCTV Calculator?
--------------------------
It is a simple calculator that allows you to quickly calculate the bandwidth of cameras in a CCTV installation.

How to use a CCTV Calculator?
-----------------------------
The program is equipped with a graphical interface "cctv_calculator.exe". 
You can also use the bandwidth calculator via the command line. In this case, you must provide: camera resolution, compression method, set image quality, number of recorded frames per second, and the number of cameras in the installation.
You call the program like this:
    cameras_bandwidth_calculator.exe --resolution D1 --compression MJPEG --fps 20 --cameras 3
Please note that the arguments with space inside must be inside the quotes, e.g. --resolution "1 Mpix".
For more info, you can call the program with --help flag.

Will there be a Linux release?
------------------------------
Yes, a Linux release will be available in the near future. At the moment, you can compile the program yourself - the source code is available at https://github.com/VedVid/cctv-calculator

Will there be a Mac release?
----------------------------
No, a Mac release is not planned. I don't have a Mac and maintaining a Mac-specific version is out of my reach. The source code is available on https://github.com/VedVid/cctv-calculator so you can try to compile the code yourself.
