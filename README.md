# Instruction for tesing the code

The windows subsystem for Linux (wsl) is an ideal tool for deploying and testing the code in Windows 10: https://docs.microsoft.com/en-us/windows/wsl/install-win10. 

After installing wsl version of ubuntu (can be found in the Microsoft store), the following steps should be followed:

* Open wsl ubuntu, create a user name and password;

* Preform the update of the wsl with the command: 
```sh
$ sudo apt update
$ sudo apt upgrade
```

 * Install the compilers and mpi library
```sh
$ sudo apt install build-essential
$ sudo apt install gfortran
$ sudo apt install mpich
$ sudo apt install cmake
```

* Check the Python version, Python 3 should be used with TELEMAC
```sh
$ python -V
```
* If not, install it with the command
```sh
$ sudo apt install python3
```
* Install the necessary packages for Python3
```sh
$ sudo apt install python3-pip
$ sudo apt install python3-numpy
$ sudo apt install python3-scipy
$ sudo apt install python3-matplotlib
```

* Normally the metis library is already compiled and included in the source code in 
```
Telemac_trunk_MCPBE_Flocculation\optionals\metis-5.1.0\libmetis\libmetis.a
```

* If the compiled version of libmetis.a does not work for you, you could follow the instruction **Install.txt** located in the folder
```
Telemac_trunk_MCPBE_Flocculation\optionals\metis-5.1.0
```

* Set up the system environment variables for TELEMAC. Here we use the editor vim shipped with ubuntu
```sh
$ vim ~/.bashrc
```
* Add the following lines at the end of the file **.bashrc**. Suppose the source code is located in *C:\Users\saaad\Source\Telemac_trunk_MCPBE_Flocculation*, a slightly different notation of the path should be used in wsl ubuntu. For example, C:\ in windows will become /mnt/c/, the rest of the path is the same, except that \ should be replaced by /.
```sh
export PATH=/mnt/c/Users/saaad/Source/Telemac_trunk_MCPBE_Flocculation/scripts/python3:$PATH
export SYSTELCFG=/mnt/c/Users/saaad/Source/Telemac_trunk_MCPBE_Flocculation/configs/systel.cis-win10-ubuntu.cfg
```

* In the above case, the configureation file **systel.cis-win10-ubuntu.cfg** is used. By default it uses a configuration called *ubugfortransdbg* (at line 7). One can add additional configurations to the same line, e.g. the configurations listed at line 8. If you want to use the parallel version, don't foget to change the line 52, in which the correct path to the libmetis.a should be given.