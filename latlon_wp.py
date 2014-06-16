#!/usr/bin/env python

import re

def decimalat(DegString):
	#'def' is how we tell python that what follows is a function
	#re module must already be imported by this point - top o'the file!
	#this will turn a "34 56.89 S" string and return decimal degrees
	SearchStr = '(\d+) ([\d\.]+) (\w)'
	Result = re.search(SearchStr, DegString)
	
	#take the captured groups from the grep, convert to floats and give them
	#sensible names...
	Degree  = float(Result.group(1))
	Minute  = float(Result.group(2))
	Compass = Result.group(3).upper() #make sure upper case!
	
	#calculate the decimal degrees
	DecimalDegree = Degree + Minute / 60
	
	#if the coord is South or West of meridian/equator then make coord -ve
	if Compass == 'S' or Compass == 'W':
		DecimalDegree = -DecimalDegree
	
	return DecimalDegree
#end of indentation ends the function.

InFileName = "Marrus_claudanielis.txt"

OutFileWrite = True

#open file for reading
InFile = open(InFileName, 'r')
OutFileName = "dec_" + InFileName

HeaderLine = 'dive\tdepth\tlattitude\tlongitude\tdate\tcomment'
print HeaderLine

if OutFileWrite:
	#if you're gonna write, open your output file
	#create output file to prepare too write to it, 'w', or append 'a'
	OutFile = open(OutFileName, 'w')
	OutFile.write(HeaderLine + '\n')

LineNumber = 0
#loop through the lines...
for Line in InFile:
	if LineNumber > 0:
		#print Line
		#remove line enders
		Line = Line.strip('\n')
		#split at the tabs
		ElementList = Line.split('\t')
		#print the line
		#print LineNumber, ':', ElementList
		Dive = ElementList[0]
		Date = ElementList[1]
		Depth = ElementList[4]
		Comment = ElementList[5]
		
		LatDegrees = decimalat(ElementList[2])
		LonDegrees = decimalat(ElementList[3])
		
		OutputString =  "%s\t%4s\t%10.5f\t%10.5f\t%9s\t%s" % \
		(Dive, Depth, LatDegrees, LonDegrees, Date, Comment)
		print OutputString
		if OutFileWrite:
			OutFile.write(OutputString + "\n")		
		
		#update the index
	LineNumber += 1

#once it's all in, close the file?
InFile.close()
if OutFileWrite:
	OutFile.close()






















