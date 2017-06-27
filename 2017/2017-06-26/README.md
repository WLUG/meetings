## Server Generator and the Client python programs

### Overview

The Server Generator program produces a template python server program. The 
server program may blindly broadcast data to the client application or supply 
data to the client on command from the client.

This server program should be modified to suite your requirements. Focus on 
having the server program capture and process data. The server template program 
is easily adapted to dispatch that data to a client.

The inter-process communication (IPC) between the server and the client is 
performed over the System D-Bus (as opposed to the Session D-Bus). 

The Linux operating systems *systemd* auto-starts the server application. The 
systemctl utility may be used to manage the server application, while the 
journalctl will report the status of the server.


## Practical Example

For a practical example a Raspberry Pi computer has General Purpose Input/Output
(GPIO) ports. A sensor in a machine monitors a rotating wheel. The sensor sends
pulses into a GPIO port. The server program counts the pulses. Every second
the server processes the number of pulses counted. It then publishes onto the 
system D-Bus this processed data. For example the data may be the calculated 
speed of the circumference of a wheel. As long as the Raspberry Pi is booted 
this data is published. The server program may also retains the value of the 
maximum speed.

When a client application is launched it subscribes and receives the regularly 
published data from the server. The client is then responsible for displaying 
this data. If the user wants the value for the maximum speed, then the client 
performs a "method call" to the server and the server returns this value.

If the client application is no longer required it can be shut-down and 
restarted at another time. Meanwhile the server will be unaware of the status
of the client and the server continues performing its task.

The server application that is generated and the client applications supplied
allow the developer to focus on the task the server should perform, without
having to devote too much time toward understanding and implementing the D-Bus 
inter-process data transfer.
 

### These programs were written to demonstrate:

* The use of pydbus library for python3 to provide system D_Bus signal emission
publishing and subscription and D-Bus method calls.

* The use of systemd to launch a local python server program on booting.

* The use of systemd's *systemctl* to start a local python server program. In 
the GUI client this is performed with subprocess.run(). It may also be performed
at the command line using systemctl start.

* A systemd .service file is provided to support systemd services to the local
server template file.

* A D-Bus configuration file is provided to modify the standard system D-Bus
configuration so that permissions are correct.

* The locally located python server template file will run as a server and 
interacts with the client programs via the system D-Bus.

* The python server template file that is created, should then be modified to 
create a server program that meets the developers specifications.


## Installation and Operation

### Prerequisites are:

* Python 3.5 or higher in order to support subprocess.run() function.

* pydbus module installed system-wide. pydbus module requires GLib at version 2.46 or higher. Recent distro's support this with: 

    `$ sudo apt-get install python3-pydbus.` 

    Otherwise to install from PyPI enter: 

    `$ sudo pip3 install pydbus --target=/usr/lib/python3/dist-packages`


### Installation:

Unzip the files into a local folder. E.g. ~/server_generator

The folders contents will be:

>   client_1

>   client_2

>   client_3

>   client_4

>   client_5

>   help_client_full.md

>   help_client.md

>   help_full_server_generator.md

>   help_server_generator.md

>   README.md

>   server_generator


Use root priv to run the server_generator. E.g.

`$ sudo server_generator`

If you accept the defaults supplied in the installation script and also 
request the starting of the server program, then the program in 
*~/project_1/server_1/app_1* will be launched.

app_1 is the server template application. It contains code that makes it
run for 100 seconds:
```
    if counter < 100:
        return True
    else:
        loop.quit()
        sys.exit()
        return False
```

Edit this code to make this program run for longer, or remove it to make it 
continuously run.

The client programs may be launched and each one adds a little more complexity
with regard to its interaction over the D-Bus with the server app_1.


### Removing template server application

To remove the template application server and its folders, the systemd service, 
and the D-Bus configuration file:

`$ sudo server_generator --remove`

The removal script will use systemctl to stop, disable and delete the systemd 
service that runs the server application. A choice is provided as to whether
or not to remove the server application and its folders, and the D-Bus
configuration file.


### Presentation

These programs were demonstrated at the Waikato Linux User Group meeting on 27 June 2017. A slide show was also delivered as an introduction. This slide show is the included file *Client Server presentation*. 
