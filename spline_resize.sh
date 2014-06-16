#! /bin/bash

# UPDATE!
# this script is now a little smarter - at the command line:
# " bash positional_resize.sh "
# ...will do the do all the rotating and cropping needed, but now:
# " bash positional_resize.sh bottom "
# ...will crop from the top, for those images where the wing would
# otherwise be vanishing off the bottom of the images, and:
# " bash positional_resize.sh top "
# ...will crop from the bottom for any images where the wing would
# otherwise be vanishing off the top edge.
# This means that if you run the script once, you can then go back
# and copy the images that are wrong into a 'Top' and/or 'Bottom'
# folder and re-run the script - I get 100% splining with this
# approach. Happy Splining! Will.


mkdir landmarking

find *.tif -exec cp {} landmarking/{} \;

cd landmarking

echo 'resizing for landmarking'

find *.tif -exec mogrify -resize '632x480' -extent '632x480' {} \;

mkdir splining

echo 'resizing for splining'

# making sure that the splining size images retain the greyscale colourspace
find *.tif -exec convert {} -colorspace Gray splining/{} \;

cd splining

find *.tif -exec mogrify -resize '50%' {} \;
# 312 by 240 for splining

echo 'files adjusted:'
ls -1 *.tif | wc -l