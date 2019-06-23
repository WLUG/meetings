# Generating .deb from Python package

* Use this approach when you want to package Python packages.
* Can make use of `pip` to download the packages.
* Point to `setup.py` when installing from local source code.
* Pulls in meta-data from Python package to fill in name, version, etc.
* Fills in Python dependencies, assuming that Debian packages corresponding 
  to Python packages exist. This will fail with packages that can only be
  installed through `pip`. In order to avoid impossible dependencies, you 
  can turn the auto-dependency-detection off (`--no-python-dependencies`)
  and specify manually what Debian dependencies to use (`-d/--depends`).

## Requirements

Requires *setuptools*:

```
sudo apt-get install python-setuptools python3-setuptools
```

## Build and test

Options:
* `-s python` - for using Python packages as input
* `-t deb` - the target is Debian package
* `-f` - overwrite any previous output
* `--python-bin ...` - the path to the `pytthon` executable (determines where to install)
* `--python-pip ...` - the path to the `pip` executable for pulling in packages
* `--no-python-dependencies` - to avoid automatic dependency detection
* `--depends ...` - manual specification of dependency (can be supplied multiple times)

Use `-v ...` to specify a specific version rather than the latest available one.

Execute the following command for Python 2.7:

```
fpm -s python \
    -t deb \
    -f \
    --python-package-name-prefix python27 \
    --python-bin /usr/bin/python2.7 \
    --python-pip /usr/bin/pip \
    --no-python-dependencies \
    --depends python-numpy \
    python-weka-wrapper
```

And for Python 3.6:

```
fpm -s python \
    -t deb \
    -f \
    --python-package-name-prefix python36 \
    --python-bin /usr/bin/python3.6 \
    --python-pip /usr/bin/pip3 \
    --no-python-dependencies \
    --depends python3-numpy \
    --depends 'python3 >= 3.6' \
    python-weka-wrapper3
```

List package information:

```
dpkg -I python27-weka-wrapper_0.3.15_all.deb
dpkg -I python36-weka-wrapper3_0.1.7_all.deb
```

Install package:

```
sudo dpkg -i python27-weka-wrapper_0.3.15_all.deb
sudo dpkg -i python36-weka-wrapper3_0.1.7_all.deb
```

Use installed Python 2.7 package:

```python
$ python2.7
>>> import weka.core.jvm as jvm
>>> jvm.start()
>>> jvm.stop()
```

```python
$ python3.6
>>> import weka.core.jvm as jvm
>>> jvm.start()
>>> jvm.stop()
```

Uninstall package:

```
sudo apt-get purge python-weka-wrapper
sudo apt-get purge python-weka-wrapper3
```

