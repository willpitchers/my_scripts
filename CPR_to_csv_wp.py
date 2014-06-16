#!usr/bin/env python

#This script is Will's version of the bookgroup homework.
#the plan is to take CPReaderer output '.dat' files and parse them into a sensible format
#with a header row, the '.asc' bullshit stripped out and the useful information parsed
#from the file name of the image...

Usage = "\nCall with the pattern: 'CPR_to_csv_wp.py your_input_filename.dat'\n\
(NB: this will overwrite if you select an existing filename)\n"

import sys

if len(sys.argv) != 2:
	print Usage
else:
	InFileName = sys.argv[1]
	#print "Specify file to parse:"
	#InFileName = raw_input()
	
	print "Specify an 'output_filename_name.csv':"
	OutFileName = raw_input()
	
	#InFileName = "Output_Landmarks+Veins+Outline.dat" #specify by hand for debugging
	InFile = open(InFileName, 'r')
	OutFile = open(OutFileName, 'w')
	
	LineNumber = 0
	
	HeaderLine = "Line,Temp,Sex,Rep,Scale,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,\
	X6,Y6,X7,Y7,X8,Y8,X9,Y9,X10,Y10,X11,Y11,X12,Y12,X13,Y13,X14,Y14,X15,Y15,\
	X16,Y16,X17,Y17,X18,Y18,X19,Y19,X20,Y20,X21,Y21,X22,Y22,X23,Y23,X24,Y24,\
	X25,Y25,X26,Y26,X27,Y27,X28,Y28,X29,Y29,X30,Y30,X31,Y31,X32,Y32,X33,Y33,\
	X34,Y34,X35,Y35,X36,Y36,X37,Y37,X38,Y38,X39,Y39,X40,Y40,X41,Y41,X42,Y42,X43\
	,Y43,X44,Y44,X45,Y45,X46,Y46,X47,Y47,X48,Y48,CSize"
	OutFile.write(HeaderLine+"\r\n")
	
	sys.stderr.write("-parsing in progress-\n")
	
	for Line in InFile:
		Line = Line.strip()
		#this delimiting line swaps out the 3tab gap where the WTF cols are empty
		LineDelimited = Line.replace("\t\t\t", "\tX\tX\t")
		ElementList = LineDelimited.split('\t')
		ListLength = len(ElementList) #should be 110
		#this is about pruning the filename out of the file address string
		ImageAddressNameList = ElementList[0].split('\\')
		AddressLength = len(ImageAddressNameList)
		ImageFileName = ImageAddressNameList[AddressLength-1]
		ImageFileName = ImageFileName.replace('.tif', '')
		#then we split the filename into its 'bits' of info
		ImageFileBits = ImageFileName.split('_')
		#...and select the bits we want to keep
		Scale = ElementList[10]
		FlyLine = 'line_' + ImageFileBits[1]
		Temp = ImageFileBits[3]
		Sex = ImageFileBits[2]
		Sex = Sex.upper()
		Rep = ImageFileBits[4]
		## HERE'S WHERE TO CHANGE SCRIPT FOR DIFFERENT IMAGE SETS
		FrontMatter = [FlyLine, Temp, Sex, Rep, Scale]
		OutLineList = FrontMatter + ElementList[13:ListLength]
		OutLine = ',' .join(OutLineList)
		OutFile.write(OutLine+"\r\n")
		#print (OutLine+"\r\n") #for debugging
		LineNumber = LineNumber + 1
	
	
	InFile.close()
	OutFile.close()
	
	sys.stderr.write("\n%d rows parsed\n"  % LineNumber)
