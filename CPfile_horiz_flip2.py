#!usr/bin/env python

#This script takes (wingmachine output) .cp files and checks their orientation.
#if the wing is orientated distal=right it does nothing, if distal=left, it 
#'flips' the splines horizontally, by subtracting the x coords from 316
#(the x dimension of the splining images) and writes the output to
#"flp_x" where x is the original filename

import sys
import re

FileList = sys.argv[1:]
FileNumber = 0

for InFileName in FileList:
	
	InFile = open(InFileName, 'r')
	
	CPFile = InFile.readlines()
	Header = CPFile[0]
	HeadList = Header.split()
	if float(HeadList[3]) < 158:
	#i.e. if the wing is already distal=right
		sys.stderr.write("%s OK\n" % InFileName)
		FileNumber += 1
	else:
	#i.e. if the wing is orientated distal = left
		InFile = open(InFileName, 'r')
		LineNumber = 0
		#then a new file needs to be written
		OutFileName = "flp_" + InFileName
		OutFile = open(OutFileName, 'w')
		for Line in InFile:
			Line = Line.strip()
			ElementList = Line.split()
			ListLength = len(ElementList)
			if ListLength == 2:
				FloatX = float(ElementList[0])
				NewX = 316 - FloatX
				ElementList[0] = str(NewX)
			OutLine = ' ' .join(ElementList)
			#print OutLine  #just to check that it's working...
			OutFile.write(OutLine+"\r\n")
			LineNumber = LineNumber + 1
		OutFile.close()
		sys.stderr.write("%s flipped\n" % InFileName)
	InFile.close()
	FileNumber += 1

sys.stderr.write ("%d files checked\n" % FileNumber)

