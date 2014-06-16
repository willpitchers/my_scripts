#! /bin/bash

# UPDATE!
# this script is now a little smarter - at the command line:
# " bash craig_image_adjust.sh "
# ...will do the same thing as before, but now:
# " bash craig_image_adjust.sh bottom "
# ...will crop from the top, for those images where the wing would
# otherwise be vanishing off the bottom of the images, and:
# " bash craig_image_adjust.sh top "
# ...will crop from the bottom for any images where the wing would
# otherwise be vanishing off the top edge.
# This means that if you run the script once, you can then go back
# and copy the images that are wrong into a 'Top' and/or 'Bottom'
# folder and re-run the script - I get 100% splining with this
# approach. Happy Splining! Will.

mkdir Rotated

echo 'rotating images'

find *.tif -exec convert -rotate +90 {} Rotated/{} \;

cd Rotated

find *.tif -exec mogrify -gravity NorthEast -crop '100x75%' {} \;

mkdir landmarking

find *.tif -exec cp {} landmarking/{} \;

cd landmarking

echo 'resizing for landmarking'

find *.tif -exec mogrify -gravity East -resize '632' {} \;

# 632 by 480 LM for landmarking

if [[ $1 = "bottom" ]]
then
	# bottom
	find *.tif -exec mogrify -crop '632x480+0+152' {} \;
elif [[ $1 = "top" ]]
then
	# top
	find *.tif -exec mogrify -crop '632x480+0+0' {} \;
else
	# centre
	find *.tif -exec mogrify -crop '632x480+0+76' {} \;
fi

mkdir splining

echo 'resizing for splining'

find *.tif -exec cp {} splining/{} \;

cd splining

find *.tif -exec mogrify -gravity East -resize '50%' {} \;
# 312 by 240 for splining

echo 'files adjusted:'
ls -1 *.tif | wc -l