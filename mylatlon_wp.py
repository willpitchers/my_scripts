#!/usr/bin/env python

import MySQLdb
from datetime import datetime
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

# open connection to the SQL Dbase
MyConnection = MySQLdb.connect( host="localhost", user="root", passwd="", db='midwater')
MyCursor = MyConnection.cursor()

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
		Depth = float(ElementList[4])
		Comment = ElementList[5]
		
		LatDegrees = decimalat(ElementList[2])
		LonDegrees = decimalat(ElementList[3])
		
		#split vehicle & dive number from 1st element
		SearchStr='(.+?) (\d+)'
		Result = re.search(SearchStr, Dive)
		Vehicle = Result.group(1)
		DiveNum = int(Result.group(2))
		
		#reformat date
		DateParsed = datetime.strptime(Date, "%d-%b-%y")
		DateOut = DateParsed.strftime("%Y-%m-%d") #string from datetime object
		
		#print Vehicle, DiveNum, DateOut, LatDegrees, LonDegrees, Depth, Comment
		
		SQL = """INSERT INTO specimens SET
		vehicle='%s',
		dive=%d ,
		date='%s',
		lat=%.4f ,
		lon=%.4f ,
		depth=%.1f ,
		notes='%s' ;
		""" % (Vehicle, DiveNum, DateOut, LatDegrees, LonDegrees, Depth, Comment)
		print SQL
		# after printing, run as if at the SQL terminal
		MyCursor.execute(SQL)
	#update the index
	LineNumber += 1

#once it's all in, close the file?
InFile.close()

# and close the Dbase connection AND SAVE CHANGES!
MyCursor.close()
MyConnection.commit()
MyConnection.close()






