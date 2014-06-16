#!usr/bin/env python

#This script takes (wingmachine output) .cp files strips out the coords for the
#control points corresponding to LM's 3 & 10 (the junction of L3 with the ACV
#and the junction of L3 with the wing margin) then it calculates the straight-line
#distance between them as an alternate measure of wing size and writes out a .csv
#file with each distance labelled by the original name of the image file.

import sys
import math

ListFile = sys.argv[1]
FileList = open(ListFile, 'r').read().splitlines()


#FileList = [ "wing1000.cp" ]

OutFile = open("L3_Lengths.csv", 'a')
OutFile.write("File,Image,WingLength\r\n")

FileNumber = 0
for InFileName in FileList:
	
	InFile = open(InFileName, 'r')
	
	LineNumber = 0
	for Line in InFile:
		Line = Line.strip()
		if LineNumber == 0:
			HeadList = Line.split()
			ImageName = HeadList[2]
			ImageName = ImageName.lower()
			ImageName = ImageName.replace('.tif','')
		elif LineNumber == 30:
			Distal = Line.split()
			DistalX = float(Distal[0])
			DistalY = float(Distal[1])
		elif LineNumber == 34:
			Proximal = Line.split()
			ProximalX = float(Proximal[0])
			ProximalY = float(Proximal[1])
		LineNumber += 1
	#sqrt of sqX + sqY = linear diff by pythagoras..
	DiffX = DistalX - ProximalX
	DiffY = DistalY - ProximalY
	DiffLinear = math.sqrt( math.pow(DiffX, 2) + math.pow(DiffY, 2) )
	#then multiply by the scaling factor: 0.004 for MS's images to
	DiffLinear = DiffLinear * 0.004
	OutList = [ InFileName, ImageName, str(DiffLinear) ]
	OutLine = ',' .join(OutList)
	OutFile.write(OutLine+"\r\n")
	#print OutLine
	InFile.close()
	FileNumber += 1

sys.stderr.write ("%d files checked\n" % FileNumber)

