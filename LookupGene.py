#!usr/bin/env python

# This script takes a genome annotation file and a list of SNPs from my GWAS pipeline and spits out a text file with all entries that match the SNPs on the list.
# THIS IN INTENDED TO BE RUN IN PYTHON ON A UNIX PLATFORM
# Eladio's chromosome coding to be replaced with Flybase-style coding <- DeMarquez function

Usage = "\nThis script is intended to look up the nearest gene for each SNP when given a list of SNPs.\n\
You should call it thus:   python LookUpGene.py annotation_file.csv snpfile.txt\n"

import sys
import subprocess

def DeMarquez( SNP ): # look up 1st 2 digits in dict, convert remainder to int to lose leading zeros..
    Chromos = { '10':'X', '21':'2L', '22':'2R', '31':'3L', '32':'3R' }
    Snp = str( SNP )
    snp = [ Chromos[ str(Snp[0:2]) ], str( int( Snp[2:10] )) ]
    return snp

line_no = 1

if len( sys.argv ) != 3:
	print Usage
	exit

else:
	AnnotationFile = sys.argv[1]
	SNPfile = sys.argv[2]

	annotationfile = open( AnnotationFile, 'r')
	snpfile = open( SNPfile, 'r')

	print "Specify an 'output_filename_name.txt':"
	OutFileName = raw_input()
	OutFile = open( OutFileName, 'w')		# read header

	OutFile.write( annotationfile.readline() )
	for Line in snpfile:
		line = Line.strip()
		line = line.split()
		if len( line ) != 1:
			sys.stderr.write( "error at line %d" %line_no )
		else:
			snp = ','.join( DeMarquez( line[0] ))
			bashcom = "grep " + snp + " " + AnnotationFile
			grepid = subprocess.Popen( bashcom, shell=True, stdout=subprocess.PIPE )
			outrow = grepid.communicate()[0]
			print outrow
			OutFile.write( str( outrow ) )
			line_no +=1

	OutFile.close()
	snpfile.close()
	annotationfile.close()

sys.stderr.write( "\n %d SNPs looked up \n" %line_no )

