#!usr/bin/env python

#This script takes a (wingmachine output) .cp file and effectively 'flips' the
#splines horizontally, by subtracting the x coords from 316 (the x dimension of 
#the splining images)


InFileName = "FILENAME.cp"
InFile = open(InFileName, 'r')

OutFileName = "flp_" + InFileName
OutFile = open(OutFileName, 'w')

LineNumber = 0

for Line in InFile:
	Line = Line.strip()
	ElementList = Line.split()
	ListLength = len(ElementList)
	if ListLength == 2:
		FloatX = float(ElementList[0])
		NewX = 316 - FloatX
		ElementList[0] = str(NewX)
	OutLine = ' ' .join(ElementList)
#	print LineNumber, ": ", ListLength, ": ", ElementList
#	print OutLine
	OutFile.write(OutLine+"\r\n")
	LineNumber = LineNumber + 1


InFile.close()
OutFile.close()