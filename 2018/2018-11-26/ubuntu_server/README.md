## Ubuntu (Alternative) Server - Adding a GUI.

Ubuntu now provide two version of the Server distro. The ubuntu-18.04.1-live-server and the *alternative* ubuntu-18.04.1-server.

After installing the ubuntu-18.04.1-server, then a GUI interface may be added to it. If you intend to permanently run in a GUI environment (i.e. with systemctl get-default as the *graphical.target*) then install a GUI using the tool *tasksel*.

While the installed GUI can be turned on and off  with the commands:

    $ systemctl set-default multi-user.target
    $ systemctl isolate graphical.target
    $ systemctl isolate multi-user.target 

...a problem exists that prevents logging into the GUI environment.


If you wish to switch the GUI on and off, then this presentation describes one method in which Lubuntu is manually installed rather than using the *tasksel* utility.

When operating as a server without a GUI then there is no auto mounting of USB drives. Two option for doing this are provided.

The wifi you connect to as part of your installation is stored in `/etc/netplan/01-netcfg.yaml`. This presentation shows how to edit this file to connect to different wifi's.


Presentation by: Ian Stewart - 26 November 2018

Copyright [CC0](https://creativecommons.org/publicdomain/zero/1.0/legalcode)


