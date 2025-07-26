# GNAT Studio launcher for macOS

This program launches [GNAT Studio](https://github.com/AdaCore/gnatstudio) executable
 inside a macOS bundle app with passing arguments and setting environment variables:

* for GTK
    * XDG_DATA_DIRS
    * GTK_EXE_PREFIX
    * GDK_PIXBUF_MODULE_FILE
    * GI_TYPELIB_PATH
* for Python
    * GNATSTUDIO_PYTHONHOME
* for GNAT Studio
    * GPS_ROOT
* for libraries
    * DYLD_LIBRARY_PATH
* for external excutables
    * PATH from Info.plist with
        * GS_GNAT_PATH
        * GS_GPR_PATH

Extra environment variables may be set from configuration file `gnatstudio_launcher.rc` located
 in GNAT Studio preferences folder, typically PATH and GPR_PROJECT_PATH.

Pascal Pignard, June 2024.

