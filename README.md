# Experiments with Clozure CL on Raspberry Pi

This repository contains experimental code that I've written while
playing with Clozure CL on the Raspberry Pi.  You may find the
directory ccl-arm-linux-headers interesting.  They contain .cdb files
for many of the Linux header files to make Linux specific
functionality accessible in the Clozure CL FFI.  If you want to use
them, create a symlink from the ccl-arm-linux-headers directory to
arm-headers/linux in your Clozure CL source directory.