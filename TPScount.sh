#! /bin/bash

cat $1/*.TPS > tempfile.txt
echo "You have measured:"
grep -c "IMAGE=" tempfile.txt
echo "individuals Sudarshan - time for lunch!"
rm tempfile.txt
