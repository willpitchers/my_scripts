#! /bin/bash

# Abhijna's backup script!
# this script can live anywhere, but make sure you put in absolute paths
# to make it work, update the paths in this script and then run:
# 'crontab -e'
# which will open your table of timed jerbs (crontab)
# add a line that reads:
# 00 * * * * bash ~/path/to/this/script/Abi_Backup.sh
# and make sure to save the file.

run once
# rsync -crv ~/Documents/AllTheData/ USERNAME@hpcc.msu.edu:/mnt/research/ninjamouse/nameofsyncfolder

rsync -e"ssh -i /path/to/privateKey" -cravR ~/Documents/AllTheData/ USERNAME@hpcc.msu.edu:/mnt/research/ninjamouse/nameofsyncfolder
