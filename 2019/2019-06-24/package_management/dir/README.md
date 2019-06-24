# Generating .deb from directory

Use this approach if you have scripts, config files other files like
documentation or .desktop files on disk that need packaging up.

## Build and test

Options:
* `-s dir` - for using a directory-based approach
* `-t deb` - the target is Debian package
* `-n ...` - the name of the package
* `-v ...` - the version of the package
* `-f` - overwrites any previous output
* `from=to` - maps source files to target files

Execute the following command:

```
fpm -s dir -n hello-world -t deb -v 0.0.1 \
      src/hello-world=/usr/bin/hello-world \
      src/hello-world.msg=/etc/hello-world/hello-world.msg
```

List content of package:

```
dpkg -c hello-world_0.0.1_amd64.deb
```

Output:

```
rwxrwxr-x 0/0               0 2019-06-23 12:56 ./
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./usr/
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./usr/share/
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./usr/share/doc/
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./usr/share/doc/hello-world/
-rw-r--r-- 0/0             140 2019-06-23 12:56 ./usr/share/doc/hello-world/changelog.gz
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./usr/bin/
-rwxrwxr-x 0/0              39 2019-06-23 12:47 ./usr/bin/hello-world
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./etc/
drwxr-xr-x 0/0               0 2019-06-23 12:56 ./etc/hello-world/
-rw-rw-r-- 0/0              13 2019-06-23 12:48 ./etc/hello-world/hello-world.msg
```

Install package:

```
sudo dpkg -i hello-world_0.0.1_amd64.deb
```

Run installed script:

```
hello-world
```

Uninstall script:

```
sudo apt-get purge hello-world
```

