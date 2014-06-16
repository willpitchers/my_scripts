#! /bin/bash

# THIS VERSION WITH ROITATION REMOVED FOR DVZ 28TH MAY 2014

# UPDATED UPDATE!! [work-around for Frankino/Stillwell images]
# this script is an attempt to do something new - the workflow will become thus:
# run once before LM-ing and it will rotate, greyscale-ify and shrink images, but
# not change aspect ratio. Then landmarking in tpsDIG happens, with dudeman.py
# afterwards. Running again after LM-ing will then use the landmark info (protected
# name?) to 'aim' the second resizing step... 
# Happy Splining! Will.


# ask user which stage we're at...
read -p "have you landmarked your images yet? (y/n)  "


if [[ "$REPLY" == "n" || "$REPLY" == "N" ]]
then
	# new directory for LM-ing images
	mkdir To_Landmark
	
	echo '' ; echo 'rotating images & resizing for landmarking'
	
	# grab all .tifs, flip+greyscale+shrink into LM-ing folder
	find *.tif -exec convert {} -colorspace Gray -gravity East -resize '632' To_Landmark/{} \;
	
	echo '' ; echo 'please landmark your images now' ; echo ''
	
elif [[ "$REPLY" == "y" || "$REPLY" == "Y" ]]
then
	# make a splining directory, copy images & .asc file in, cd into new dir...
	mkdir To_Spline
	cp *.tif To_Spline/
	cp landmarks.asc To_Spline/landmarks.asc
	cd To_Spline
	
	echo '' ; echo 'adaptive cropping'
	
	# ...and here's the 1st active ingredient: awk takes the 1st & 3rd column from 
	# each line (filename & Y1) and re-rerubs them into a mogrify command...
	awk '{Image=$1} { if ($3<-240) {Yoffset=0} else {Yoffset=$3+240} } \
		{Coords="632x480+0+"Yoffset} {system("mogrify -crop " Coords " " Image)}' landmarks.asc
	
	echo '' ; echo 'resizing for splining'
	
	# this is standard-issue shrink by 1/2 command applied to all images for splining
	find *.tif -exec mogrify -type Grayscale -resize '50%' -depth 8 {} \;
	
	echo '' ; echo 'accomplishing landmark voodoo'
	
	# active ingredient the 2nd: this awk needs to pull each .asc line, adjust BOTH Y 
	# coords and write out to a new file...
	awk ' function abs(x){return (((x < 0.0) ? -x : x) + 0.0)} \
	 { if ($3<0) { Y1=843-(abs($3)+480)-(240+$3) } else { Y1=843-(480-$3)-(240+$3) } } \
	 { if ($5<0) { Y2=843-(abs($5)+480)-(240+$3) } else { Y2=843-(480-$5)-(240+$3) } } \
	{print $1" "$2" "Y1" "$4" "Y2" "$6" "$7" "$8" "$9" "$10" "$11}' landmarks.asc > new.asc
	
	# replace old lm file with the new
	mv new.asc landmarks.asc
	
	echo '' ; echo "you're ready for WingMachine!" ; echo ''

else
	echo "What!? Don't you know? ...are you sure that you ought to be using this script?"
fi