# Peter's set of command-line tools

The following video is a good introduction into some useful commands
around a terminal and bash:

https://www.youtube.com/watch?v=AVXYq8aL47Q

[Here](bash.md) is a very short summary of the content.


## ls

single column

```
ls -1 
```

newest file first (long format)

```
ls -ltr
```

`--full-time` - nano-second resolution


## head/tail

print start/end of a text file ([iris.csv](iris.csv))

```
head -n1 iris.csv
```

```
tail -n10 /var/log/dpkg.log
```

`tail -f FILE` - "follow", ie output as file gets appended (great for log files!)


## wc - word count

```
wc -l iris.csv
```

* `-l` lines
* `-c` chars
* `-b` bytes
* `-w` words


## column

Turn output into nice tables via separator

```
column -s, -t iris.csv
```


## bc - An arbitrary precision calculator language

```
echo "2+5" | bc
```

with math library (-l) for functions like sine:

```
echo "s(1)" | bc -l
```


## grep

find files with certain content

```
grep -inH "versi" *
```

* `-n` with line number
* `-H` with file name
* `-i` ignore case


## find

find files in directories

find /home/fracpete/documents/wlug/meetings -name `*.md`

* `-name "GLOB"` - files matching the glob
* `-iname "GLOB"` - case-insensitive matching of glob
* `-type f` - files
* `-type d` - directories
* `-exec CMD {} \;` - spawn CMD with `{}` being the located file/dir


