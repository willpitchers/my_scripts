#!/usr/bin/env python
#practice program from PracCompBiol chapter 8 - asks for a sequence to be
# input and returns length and percentage composition by nucleotide
# here's a hard-coded test string... comment it out when finished:
DNASeq = "ATCTCTCATTCAAACGA"
#DNASeq = raw_input('Enter a sequence: ')
DNASeq = DNASeq.upper().replace(" ","")
# convert all to uppercase and remove any spaces...
print 'Sequence:', DNASeq
SeqLength = float(len(DNASeq))
#this must be float so that the proportions don't get truncated to 0!
print 'Sequence Length:', SeqLength

NumberA = DNASeq.count('A')
NumberC = DNASeq.count('C')
NumberG = DNASeq.count('G')
NumberT = DNASeq.count('T')

print 'A: %.1f' % (100 * NumberA/SeqLength)
print 'C: %.1f' % (100 * NumberC/SeqLength)
print 'G: %.1f' % (100 * NumberG/SeqLength)
print 'T: %.1f' % (100 * NumberT/SeqLength)
#calculate percetage compositions and return at .1 decimal place precision
TotalStrong = NumberG + NumberC
TotalWeak = NumberA + NumberT
if SeqLength >= 14:
  #formula for longer sequences
  MeltTempLong = 64.9 + 41 * (TotalStrong - 16.4) / SeqLength
  print "Tm Long (>14): %.1f C" % (MeltTempLong)
else:
  #formula for shorter sequences 
  MeltTemp = (4 * TotalStrong) + (2 * TotalWeak)
  print "Melting Temp : %.1f C" % (MeltTemp)
