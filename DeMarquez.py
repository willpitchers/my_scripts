#!

# a very simple script to make the DeMarquez function available elsewhere – Eladio's chromosome coding to be replaced with Flybase-style coding

import sys

snps = sys.argv[1:]

def DeMarquez( SNP ): # look up 1st 2 digits in dict, convert remainder to int to lose leading zeros..
    Chromos = { '10':'X', '21':'2L', '22':'2R', '31':'3L', '32':'3R' }
    Snp = str( SNP )
    snp = [ Chromos[ str(Snp[0:2]) ], str( int( Snp[2:10] )) ]
    return snp

snpsnp = [DeMarquez( SNP ) for SNP in snps]

print snpsnp
