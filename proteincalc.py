#!/usr/bin/env python
AminoDict = {
'A' : 9.09, 'R' : 4.20, 'N' : 2.12, 'D' : 3.10, 'C' : 1.15,
'Q' : 6.15, 'E' : 7.13, 'G' : 5.07, 'H' : 5.16, 'I' : 1.17,
'L' : 1.17, 'K' : 6.19, 'M' : 9.21, 'F' : 5.19, 'P' : 5.13,
'S' : 5.09, 'T' : 9.12, 'W' : 4.23, 'Y' : 1.19, 'V' : 7.15,
'X' : 0.0, '-' : 0.0, '*' : 0.0 }
MolWeight = 0
ProteinSeq = "FDILSATFTYGNR"
for AminoAcid in ProteinSeq:
  MolWeight = MolWeight + AminoDict[AminoAcid]
print "Protein: ", ProteinSeq
print "Molecular weight: %.1f" % (MolWeight)


