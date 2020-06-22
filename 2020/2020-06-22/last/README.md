# Analysis of **last** output

## Description

Linux systems maintain a /var/log/wtmp file monitoring ssh logins and reboots. They also maintain a 
/var/log/btmp file of attempted logins.

These files may be copied from a remote system and then locally decoded with the utility **last**.

The output from **last** analysis may be directed to a text file.

Ian demonstrated his python program *login_attempt_ip_address.py* which is designed to read the *last* 
text file and provide a summary of the attempted login statistics. The focus of the program is on retrieving
summaries from the btmp data. 

This program makes uses the python module **paramiko** to copy the btmp or wtmp files from a remote server. 
It is likely that you will need to add the *paramiko* module to your system with the command 
`$ sudo apt install python3-paramiko` or `$ pip3 install paramiko`.

The python program is the file:

* login_attempt_ip_address.py

The slide show presentation that was delivered is included as the files:

* last presentation.odt
* last_presentation.pdf

An example of the command to run the program and the output for a btmp file is given below.

```
$ python login_attempt_ip_addresses.py --server 111.222.222.111 --port 22 --username aroha --file /var/log/btmp

This program is designed to use the paramiko modules ssh to copy wtmp or btmp 
files from a remote Linux server so they may be locally analysed.

A prerequisite to using this program is to have previously performed a ssh
connection so that the ~/.ssh/known_hosts file contains an entry for the
remote server. E.g. $ ssh root@219.89.205.100 -p 22 

The first time this is done you are prompted to save the ssh-rsa in 
~/.ssh/known_hosts.  

Note that a wtmp / btmp file from a previous month may have a path and file name
of /var/log/btmp.1 and have r access for root and group, but not world.
Thus you may need to log in via ssh and set $ sudo chmod 664 /var/log/btmp.1
before using this program, so that the file may be accessed in order to be 
copied.


A connection and file transfer will be attempted to...
Server: 111.222.222.111  Port: 22  Username: aroha  File: /var/log/btmp

Do you wish to proceed with connect and transfer? [Y/n]: 

Enter account password for remote server: 

Connecting to remote server and copyong file...
Using 'last' to convert file to text...
Analysis by 'last' of btmp output to btmp.txt.
Analyzing the text data...
Total Usernames: 14744
Corrupted Usernames: 1602
Total failed ssh logins for the month: 200772
Max attempts from one ip address: 1026
Total number of ip addresses performing attempts: 3922
Most popular Username: root with 89106 attempts.
2020-06-01 Mon: 11437
2020-06-02 Tue: 10469
2020-06-03 Wed: 9965
2020-06-04 Thu: 7625
2020-06-05 Fri: 4975
2020-06-06 Sat: 6219
2020-06-07 Sun: 5729
2020-06-08 Mon: 8466
2020-06-09 Tue: 10694
2020-06-10 Wed: 12536
2020-06-11 Thu: 14352
2020-06-12 Fri: 9828
2020-06-13 Sat: 11689
2020-06-14 Sun: 8270
2020-06-15 Mon: 9643
2020-06-16 Tue: 15531
2020-06-17 Wed: 10471
2020-06-18 Thu: 13652
2020-06-19 Fri: 6470
2020-06-20 Sat: 6272
2020-06-21 Sun: 5577
2020-06-22 Mon: 902
2020-06-23 Tue: 0
2020-06-24 Wed: 0
2020-06-25 Thu: 0
2020-06-26 Fri: 0
2020-06-27 Sat: 0
2020-06-28 Sun: 0
2020-06-29 Mon: 0
2020-06-30 Tue: 0

```
