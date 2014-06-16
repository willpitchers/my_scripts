#!/usr/bin/env python

Usage= """
This script takes 2 or more 'X-Y' type tab-delimited text files and 
stiches them together 'XX-YY' -wise... if you call it a cockbadger
then it will retaliate in kind!
Example script from PCFB chapter 11
Usage:
	filestoXXYY_wp.py *.txt > combined_filename.txt
"""

import sys

if len(sys.argv)<2:
	print Usage
else:
	FileList= sys.argv[1:]
	Header = "Lambda"
	LinesToSkip = 1
	Delimiter = '\t' #change here if files are comma-delimited etc.
	MasterList = []
	FileNum = 0
	
	for InFileName in FileList: #this happens once per file
		sys.stderr.write("Processing file %s\n" % (InFileName))
#		print InFileName
		Header += '\t' + InFileName
		InFile = open(InFileName, 'r')
		LineNumber = 0 #reset for each file
		RecordNum = 0 #the record number within the table
		
		for Line in InFile:
			if LineNumber > (LinesToSkip-1) and len(Line)>3:
				Line = Line.strip('\n')
				if FileNum == 0:
					MasterList.append(Line)
				else:
					ElementList = Line.split(Delimiter)
					if len(ElementList)>1:
						MasterList[RecordNum] += ('\t' + ElementList[1])
						RecordNum += 1
					else:
						sys.stderr.write("Line %d not in XY format in file %s\n" \
						% (LineNumber, InFileName))
			LineNumber += 1 # the end of the Line/InFile loop
		FileNum += 1 
		InFile.close() #the end of the file loop
	#print out the results
	print Header
	for Item in MasterList:
		print Item
	
	sys.stderr.write("Converted %d files(s)\n" % FileNum)







