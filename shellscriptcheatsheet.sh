##examples from life

###'sed' reminder
sed 's/thing_to_replace/replacement/g' in_this_file.py | python
#or, for the HPCC
sed 's/thing_to_replace/replacement/g' in_this_file.R < R --no-save


#find a bunch of (e.g. .cp) files and use are args to a script
find *.cp -exec bash ~/scripts/CPflip.sh {} \;
find . -name "RAL*.tif" -exec cp {} ~/new/file/location \;


###'awk' related fun and games - for extracting the '$1'st & '$2'nd fields
awk -F "\"*,\"*" '{print $1,",",$2}' file.csv > newfile.csv
#here's how I grabbed the p.vals for making pretty plots
awk -F "\"*,\"*" '{print $2,",",$4,",",$5,",",$6,",",$7,",",$106}' specific_assoc.csv


#### examples from PCfB chapter 5

# save all reports in one text file:
curl "http://www.wunderground.com/history/airport/MIA/1992/08/[01-30]/DailyHistory.html?&format=1" >> miamiweather.txt 

# save each month's report in its own numbered file 
curl "http://www.wunderground.com/history/airport/MIA/1979/[01-12]/01/DailyHistory.html?&format=1" -o Miami_1979_#1.txt



#### examples from chapter 6

# the lines to set your path in ~/.bash_profile
export PATH="$PATH:$HOME/scripts"
set -o noclobber

# general crossref search format
http://www.crossref.org/openurl/?title=Nature&date=2008&volume=452&spage=745&pid=ourl_pcfbdemo%3Apcfbdemo 

# reformatted for a regular expression
curl "http://www.crossref.org/openurl/?title=\1\&date=\2\&volume=\3\&spage=\4\&pid=ourl_pcfbdemo%3Apcfbdemo\&redirect=false\&format=unixref"


#### ImageMagick tricks

#horizontal image flipping
mogrify -flop UN01_M_05_crop.tif

#full-size image right aligned 75% crop
convert UN01_M_05.tif -crop 75%x+350+100 UN01_M_05_crop.tif
#full-size image centered 75% crop
convert C016_M_01.tif -gravity Center -crop 75%\! C016_M_01.tif


