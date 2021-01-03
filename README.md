# VBoxManage Helper

## Overview
This is a utility primarily meant for the "scambaiting" community that allows you to change the values of fields that commonly expose the usage
of a virtual machine (specifically VirtualBox) within built-in Windows tools like msinfo32, dxdiag and WMIC.

There are many other similar programs that also do this task, with this one specifically being based upon a Batch variant by
[JayMontana36](https://github.com/JayMontana36/vBoxSysInfoMod).

This program is cross-platform across Windows, Linux, Mac OSX, pre-compiled executables are provided for those three platform as well
(you could also compile it other Unix variants if you needed to).

The underlying VirtualBox CLI tool that this program specifically calls to change field values is `VBoxManage setextradata`, you can read
Oracle's official documentation about changing these fields [here](https://docs.oracle.com/en/virtualization/virtualbox/6.0/admin/changedmi.html).

## Compiling and Usage
Usage is quite self-explanatory, as there are no CLI arguments, the program gives you selectable options and walks you through each step, much like JayMontana's version.

This program was written in Pascal and can be compiled using the [Free Pascal Compiler](https://www.freepascal.org/). There are no external dependencies you need to install, so you should be able to compile it without issue using `fpc vboxmanage_helper.pas`.
