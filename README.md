# cpup

`cpup` takes a source directory and a target directory and copies files from the
source directory to the target directory, if

* the file exists in both directories
* the modification time of the file in the source directory is more recent than
  the modification time of the file in the target directory

To use `cpup` on file systems that are case insensitive, you should set the top
level expression `respectCase` to `False`.
