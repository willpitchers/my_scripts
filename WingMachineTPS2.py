#New for version 2
#TPS dig actually has an inverted y axis compared to ImageJ
#Now we need to break up some bits, then do some math, then spit it back out
#We also strip zeros by doing this. THis is helpful according to will

#David Tack for Will Pitchers, Dec 05, 2010
#Derp.
#The philosophy here is that there is a repeating pattern of 5 units for each set of landmarks we want to associate with 
#a single wing, therefore, we will read the file as one giant string, then break it into those 5 bit parseable units, then parse information
#out of the chunks.

#We want to be proper python programmers, so we should keep everything in discrete functions.
#Thus we will first define a function, and in this case, use a WHILE loop.
#You could easily use a FOR loop by taking the len(broham) and finding out the length on a file by file basis, then itterate over that.
#Each cycle of the WHILE loop, we will take a part of the big string that is == to one set of measurements,
#break it up, then output it as a single line.
#Then we will remove those parts from the larger list and continue until there are no parts left.

#Will asked that the TIME be 'AUTOMAGIC' - this will require importing a module.
import time
a = time.localtime()
jikan = str(a[1]) +'_'+ str(a[2]) + '_' +str(a[0])
jikan2 = str(a[3]) + ':' +  str(a[4]) + ':' + str(a[5])
#this enables us to get both time fields! We have both the DATE, and the TIME of day! Woot!
#If you want to know more about modules, look it up. Lewls.
#Functions

def tps(inputfile,outputfile):
  '''Takes the outputfile from TPSDIG and pipes it into Wingmachine as an .asc file'''
  f1 = open(inputfile,'r')
  f2 = open(outputfile,'w')
  broham = f1.readlines()
  while len(broham) != 0:
    dudeman = broham[0:5]
    filename = dudeman[3].strip()
    filename = filename[6:]
    q1 = dudeman[1].strip()
    lystone = q1.split(' ')
    exone = str(int(float(lystone[0])))
    whyone = str(480 -int(float(lystone[1])))
    bit1 = exone + ' ' + whyone
    
    
    q2 = dudeman[2].strip()
    lysttwo = q2.split(' ')
    extwo = str(int(float(lysttwo[0])))
    whytwo = str(480 -int(float(lysttwo[1])))
    bit2 = extwo + ' ' + whytwo
    

    
    brohiem = filename +' ' +bit1 + ' ' + bit2
    f2.write(brohiem + extratrash + '\n')
    broham = broham[5:]
  f1.close()
  f2.close()

###Work Area###
file1 = raw_input("The File you want to fix: ")
file2 = raw_input("What you want to name the fixed file: ")
user = raw_input("Your Name: ")
date = jikan   #raw_input("The Date: ")
date2 = jikan2 #raw_input("Time of Day: ")
species = raw_input("Species: ")
gender = raw_input("Sex: ")
scale = raw_input("Scale: ")
extratrash = " " + user + " " + date +" " + date2 + " " + species + " " + gender  + " " + scale

tps(file1,file2)