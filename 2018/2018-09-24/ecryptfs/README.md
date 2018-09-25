## eCryptfs

**E**nterprise **Crypt**ographic **F**ile**s**ystem

website: [https://ecryptfs.org](URL)

eCryptfs was initially released in 2006 and was introduced into Ubuntu in April 2009.

As of ubuntu release 18.04 eCryptfs is not included in the distribution .iso file.

The following presentations are a series of screenshots taken while using eCryptfs and upgrading ubuntu to 18.04 to verify eCryptfs remains functional, etc.


### Ubuntu 17.10 (and before) 

File: Ubuntu 17.10 Home Folder Encryption.odp

Steps:

* Boot a USB stick with Ubuntu Mate 17.10.
* Perform installation to Hard Disk Drive (HDD).
* During install create *ian* account with encryption.
* Finish install and boot HDD.
* Create *aurora* account with encryption.
* Demo. Ian cant see Aurora’s files and vice versa.
* Boot a Ubuntu Mate 17.10 USB stick.
* Access the HDD.
* Decrypt /home/ian/… folders and files.
* Decrypt home/aurora/… folders and files.



### Ubuntu 17.10 online upgrade to 18.04 

File: Ubuntu 17.10 online upgrade to 18.04.odp

Steps:

* Upgrade 17.10 with latest patches.
* Perform online upgrade to 18.04.1
* Log into *ian* and see if *aurora* still has encryption.
* Create *james* account with encryption.
* Log into *james* account for the first time.
* From *ian* account on HDD try decrypting all accounts.
* Boot 18.04 USB stick / Live CD and try decrypting all acounts.

Presentation by: Ian Stewart.

Date: 24 September 2018.


