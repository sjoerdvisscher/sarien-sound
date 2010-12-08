Sarien Sound
============

This program converts a bunch of AGI sound files to AIFF files.
The default is to create one AIFF file for each AGS file.
With the --all flag the program creates one big AIFF file, together with
a JSON file which contains the offsets and length of each of each of the
original sounds in the final AIFF file (in seconds).

Usage:
------

    ags2aiff [--all] path-to-dir