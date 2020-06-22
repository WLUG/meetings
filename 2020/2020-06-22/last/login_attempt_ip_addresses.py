#!/usr/bin/env python3
#!
# login_attempt_ip_address.py
#
# Connect to a remote server and copy a wtmp or btmp file to local computer.
# Uses paramiko module. https://github.com/paramiko/paramiko
# 
# Use 'last' to convert the file to text.
#
# Analyze the text file, and output some statistics.
# Not much to analyze on a wtmp file, best to just review the 'laast' output
#
# Ian Stewart - June 2020
# 
import sys
import os
import paramiko
import subprocess
import base64
import datetime
import argparse
import getpass


SERVER = ""
PORT = 22
USERNAME = ""
FILE = "/var/log/btmp"
PYTHON_VERSION_MIN = (3, 7, 0)

# datetime.datetime.fromisoformat() requires V3.7+
# sys.version_info(major=3, minor=8, micro=2, releaselevel='final', serial=0)
if sys.version_info < PYTHON_VERSION_MIN: 
    print("Python must be at Version {}.{} or higher."
        .format(PYTHON_VERSION_MIN[0], PYTHON_VERSION_MIN[1]))
    sys.exit("Exiting...")
    
    
def copy_from_remote_server():
    """
    Copy a file from a remote server.
    Global variables read: server, port, username, password, remotepath, localpath
    """    
    ssh = paramiko.SSHClient() 
    ssh.load_host_keys(os.path.expanduser(os.path.join("~", ".ssh", "known_hosts")))
    
    try:
        ssh.connect(args.server, args.port, args.username, password)
    except Exception as e:
        print("Error: {}".format(e))
        return -1
        # paramiko.ssh_exception.AuthenticationException: Authentication failed.
        
    sftp = ssh.open_sftp()
    #sftp.put(localpath, remotepath)
    sftp.get(remotepath, localpath)
    sftp.close()
    ssh.close()
    return 0
    

def convert_to_text_file():
    """
    Use the bash "last" command to convert btmp or wtmp files to text. e.g.
    $ last --hostlast --fullnames --time-format iso --file btmp.1 OR
    $ last --hostlast --fullnames --time-format iso --file wtmp
    Use iso time as it contains the year. Other formats lack the year.
    subprocess.run() - requires Python 3.5+
    
    If btmp file, then output is:
    proteomics ssh:notty    2020-05-24T06:56:52+12:00 - 2020-05-24T06:56:52+12:00  (00:00)     199.199.199.199
    prueba   ssh:notty    2020-05-24T06:56:52+12:00 - 2020-05-24T06:56:52+12:00  (00:00)     199.199.199.199
    """

    cmd = ["last", "--hostlast", "--fullnames", "--time-format", "iso", 
           "--file", localpath]    

    localpath_text = localpath + ".txt"

    with open(localpath_text, "w") as outfile:
        subprocess.run(cmd, stdout=outfile)

    print("Analysis by 'last' of {} output to {}."
            .format(localpath, localpath_text))
    

def decimal_to_ip(ip_decimal):
    """
    Submit a decimal ip value between 0 and 2*32 - 1.
    Returns the ip_address as a zeros padded string: nnn.nnn.nnn.nnn
    If ip_decimal is invalid returns -1
    """
    
    # Test it is decimal and in range, else return -1
    try:
        ip_decimal = int(ip_decimal)
    except ValueError as e:
        print("ValueError: {}".format(e))
        return -1
    if ip_decimal < 0 or ip_decimal > 4294967295: #(2**32 - 1)
        return -1
    
    # Convert. E.g. 511 returns 000.000.001.255
    s = ""
    for i in reversed(range(4)):  
        s += "{:>03}.".format(ip_decimal // 256**i)
        ip_decimal = ip_decimal % 256**i   
    return s[:-1]

    """
    # To check function: decimal_to_ip()
    test_data = [0,1,255,256,511,512,4294967294,4294967295,-1,4294967296,"qwerty"]
    for i in range(len(test_data)):
        print("{}: {}".format(test_data[i], decimal_to_ip(test_data[i])))
    sys.exit()
    """


def ip_to_decimal(ip_address):
    """
    Convert an ip address string to decimal.
    """
    ip_list = ip_address.split(".")
            
    ip_decimal = (256**3 * int(ip_list[0]) + 
                  256**2 * int(ip_list[1]) + 
                  256**1 * int(ip_list[2]) +
                  256**0 * int(ip_list[3]))

    return ip_decimal
    

def btmp_analysis():
    """
    Unsucessful login analysis. btmp file
    Review the btmp.txt file. Some usernames may have resulted in a newline.
    For a reference use: ssh:notty
                       1         2         3         4         5         6         7         8         9  
             012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345
    prueba   ssh:notty    2020-05-24T06:56:52+12:00 - 2020-05-24T06:56:52+12:00  (00:00)     199.199.199.199
    proteomics ssh:notty    2020-05-24T06:56:52+12:00 - 2020-05-24T06:56:52+12:00  (00:00)     199.199.199.199
    administration ssh:notty    2020-05-31T06:00:59+12:00 - 2020-05-31T06:01:15+12:00  (00:00)    199.199.199.199  

    """
    # Dictionary of days of the month and the decimal ip
    day_dict = {}
    for i in range(32):
        day_dict[i] = 0    

    # Get the start of the month    
    localpath_text = localpath + ".txt"
    with open(localpath_text, "r") as fin:
        last_line = fin.readlines()[-1].strip()   
    #print(last_line)  # btmp begins 2020-06-01T00:00:17+12:00
    time_str = last_line.split(" ")[-1]
    #print(time_str)  # 2020-06-01T00:00:17+12:00

    start_of_month_timestamp = int(datetime.datetime.timestamp(
                                   datetime.datetime.fromisoformat(time_str)))
    
    # Create daily time stamps list for a month.    
    #print(start_of_month_timestamp)
    # $ date --date='@1588248000'  #Fri 01 May 2020 00:00:00 NZST
    
    time_stamp_day_list = [start_of_month_timestamp]
    for i in range(32):
        time_stamp_day_list.append(time_stamp_day_list[-1] + 86400)
                
    # Dictionary of usernames used to attempt login and number of times tried.
    username_dict = {}
        
    # Dictionary using decimal ip addresses as the keys, with timestamp list
    ip_dict = {}
    ip_address_list = []
    total_attempts = 0
    corrupt_username_count = 0
    localpath_text = localpath + ".txt"
    with open(localpath_text, "r") as fin:
        for i, line in enumerate(fin.readlines()):
            #print(i)
            position = line.find("ssh:notty")
            
            # Maybe a line with only the username that contains a newline
            if position == -1:
                corrupt_username_count += 1
                #print(line)
                continue

            # Total attempts at logging in.
            total_attempts += 1
                
            # Retrieve the iso time string and convert to an epoch timestamp integer
            time_str = line[position + 13: position + 38]
            timestamp = int(datetime.datetime.timestamp(
                            datetime.datetime.fromisoformat(time_str)))

            # Increment the day counters
            day_number = (timestamp - start_of_month_timestamp) // 86400
            day_dict[day_number] += 1


            # Retrieve the ip address string to end of line and convert to decimal integer
            ip_address = line[position + 80:].strip()
            ip_decimal = ip_to_decimal(ip_address)

            # Build the dictionary. ip_decimal is key, value is list of timestamps
            if ip_decimal not in ip_dict:
                ip_dict[ip_decimal] = [timestamp]
            else:
                ip_dict[ip_decimal].append(timestamp)

            # Usernames count dictionary
            username = line[0:position - 1].strip()
            if username not in username_dict:
                username_dict[username] = 1
            else:
                username_dict[username] += 1
                
                
            # TODO: days of the month hit rates.
            
                        
    print("Total Usernames:", len(username_dict))
     
    print("Corrupted Usernames:", corrupt_username_count)
    
    # Retrieve statistics from ip_dict        
    max_attempts = 0
    count_ip = 0
    for key, item in sorted(ip_dict.items()):
        ip_list = decimal_to_ip(key)
        #print(ip_list, len(item))
        
        if len(item) > max_attempts:
            max_attempts = len(item)
        count_ip += 1
          
    print("Total failed ssh logins for the month:", total_attempts)  
    print("Max attempts from one ip address:", max_attempts)  # 8764  199.199.199.199 8764
    print("Total number of ip addresses performing attempts:", count_ip)
    
    max_username_list = []
    max_username_value = 0
    
    for key, value in sorted(username_dict.items()):
        #print(key, str(value))

        if value > max_username_value:
            max_username_value = value
            max_username_list = [key, value]
    #print(sorted(username_dict))

    print("Most popular Username: {} with {} attempts."
            .format(max_username_list[0], max_username_list[1]))

    #print(day_dict)
    #time_stamp_day_list

    for key, item in sorted(day_dict.items()):
        timestamp = time_stamp_day_list[key]
        local_time = datetime.datetime.fromtimestamp(timestamp)
        print(local_time.strftime("%Y-%m-%d %a:"), item)
        
        
    # TODO: Stack of 10 most popular usernames tried
    # TODO: Delete .txt file upon completion.

def wtmp_analysis():            
    """
    Not much analysis to be done on wtmp file. Just output as it is.
    Sucessful logins analysis. wtmp file.
    $ last --hostlast --fullnames --time-format iso --file wtmp
    me       pts/0        2020-06-16T21:17:49+12:00 - 2020-06-16T21:17:56+12:00  (00:00)     111.222.222.000
    root     tty1         2020-06-16T16:05:09+12:00 - 2020-06-16T16:05:48+12:00  (00:00)
    reboot   system boot  2020-06-16T15:52:45+12:00   still running                          4.19.0-9-amd64

    For a reference1 find " 20" as first characters in first iso date.
    Reference 2 is first space.
    
                                   1         2         3         4         5         6         7         8         9  
                         012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345
    reboot   system boot  2020-06-16T15:52:45+12:00   still running                          4.19.0-9-amd64
    root     tty1         2020-06-16T15:45:06+12:00 - down                       (00:00)
    reboot   system boot  2020-06-16T15:44:32+12:00 - 2020-06-16T15:45:15+12:00  (00:00)     4.19.0-9-amd64
    me       pts/0        2020-06-16T09:19:58+12:00 - 2020-06-16T15:44:08+12:00  (06:24)     111.222.222.000             
    """    
    username_dict = {}
    
    ip_dict = {}
    ip_address_list = []
    total_attempts = 0
    corrupt_username_count = 0
    localpath_text = localpath + ".txt"
    with open(localpath_text, "r") as fin:
        for i, line in enumerate(fin.readlines()):
            #print(i)
            position = line.find(" 20")
            
            # Maybe a line with only the username that contains a newline
            if position == -1:
                corrupt_username_count += 1
                #print(line)
                continue

            # Total logins / reboots.
            total_attempts += 1

            # Retrieve the iso time string and convert to an epoch timestamp integer
            time_str = line[position + 1: position + 26]
            timestamp = int(datetime.datetime.timestamp(
                            datetime.datetime.fromisoformat(time_str)))
     
            #print(timestamp)

            # Get ip address
            line_list = line.split(" ")
            ip_address = line_list[-1].strip()
            
            #May end with something like 4.19.0-9-amd64
            if len(ip_address.split(".")) != 4:
                continue
                
            ip_decimal = ip_to_decimal(ip_address)
            if ip_decimal not in ip_dict:
                ip_dict[ip_decimal] = [timestamp]
            else:
                ip_dict[ip_decimal].append(timestamp)
            
    # Retrieve statistics from ip_dict        
    #      060.234.107.116 29
    print("\nIP Address        Total Logins")
    for key, item in sorted(ip_dict.items()):
        ip_list = decimal_to_ip(key)
        print(ip_list,":", len(item))
                
            
def main():

    print("\nConnecting to remote server and copying file...")
    return_code = copy_from_remote_server()

    #print(return_code)
    
    print("Using 'last' to convert file to text...")
    convert_to_text_file()
    
    # Analyze the btmp or wtmp file
    if "btmp" in args.file:
        print("Analyzing the text data...")
        btmp_analysis()
        
    if "wtmp" in args.file:
        print("Analyzing the text data...")
        wtmp_analysis()    

    sys.exit()   
    
        
if __name__ == "__main__":
    pass
    
MESSAGE = """
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

"""

print(MESSAGE)

# Use argparse for: server, port, username and path/file/
parser = argparse.ArgumentParser()


parser.add_argument("-s", "--server", 
                    type = str, 
                    default = SERVER,
                    help = "Internet name or ip address of remote server.")
                    
parser.add_argument("-p", "--port", 
                    type = int, 
                    default = PORT,
                    help = "Secure Shell port number used on remote server. "
                           "E.g. 22")                        

parser.add_argument("-u", "--username", 
                    type = str, 
                    default = USERNAME,
                    help = "Account username to login to remote server.")
                 
parser.add_argument("-f", "--file", 
                    type = str, 
                    default = FILE,
                    help = "Path and File name for remote servers. " 
                           "E.g. /var/log/wtmp")

                     
args = parser.parse_args()

if args.server == "":
    args.server = input("\nEnter the name or ip address of remote server: ")
    
if args.username == "":
    args.username =  input("\nEnter the account username on remote server: ")       
    
print("\nA connection and file transfer will be attempted to...")
print("Server:", args.server, " Port:", args.port, " Username:", 
        args.username, " File:", args.file)
        
response = input("\nDo you wish to proceed with connect and transfer? [Y/n]: ")
if response == "":
    response = "y"
if response.lower()[0] not in ["y", "1", "t"]:
    print("Please restart with --help to review the command line options.")
    print("To options at command line: --server, --port, --username, --file")
    sys.exit("Exiting...")

# Prompt for password so it doesn't remain in command line history.
password = getpass.getpass("\nEnter account password for remote server: ")
# print(password)

remotepath = args.file
localpath = os.path.basename(remotepath)

#print(remotepath, localpath)

main()
sys.exit()

"""
Example:

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

"""
