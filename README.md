# VBoxManage Helper

## Overview
This is a utility primarily meant for the "scambaiting" community that allows you to change the values of fields that commonly expose the usage
of a virtual machine (specifically VirtualBox) within built-in Windows tools like msinfo32, dxdiag and WMIC.

There are many other similar programs that also do this task, with this one specifically being based upon a Batch variant by
[JayMontana36](https://github.com/JayMontana36/vBoxSysInfoMod).

This program is intended to be cross-platform (Windows, Linux, Mac OSX), but is currently having some difficulties with running on Windows
(values for fields that contain spaces are not passed to VBoxManage correctly), so I recommend using JayMontana's Batch version above in that case.

The underlying VirtualBox CLI tool that this program specifically calls to change field values is `VBoxManage setextradata`, you can read
Oracle's official documentation about changing these fields [here](https://docs.oracle.com/en/virtualization/virtualbox/6.0/admin/changedmi.html).

## Compiling and Usage
Usage is quite self-explanatory, as there are no CLI arguments, the program gives you selectable options and walks you through each step, much like JayMontana's version.

This program was written in Pascal and can be compiled using the [Free Pascal Compiler](https://www.freepascal.org/). The program source code file
is the one with the `.pas` file extension, you can compile it using `fpc vboxmanage_helper.pas`.
