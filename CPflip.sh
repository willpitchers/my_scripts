#! /bin/bash

#the idea here is to write a bash script to find all the .CP files
#this script takes 1 argument ($1) which needs to the full filename of the .CP file which
#then sed's into the python script that does the flip and passes it to python.

sed "s/FILENAME.cp/$1/g" ~/scripts/CPfile_horizontal_flip.py | python

# for multi-file use, call:
#find *.cp -exec bash ~/scripts/CPflip.sh {} \;