#!/bin/bash
location=..
results=results
experiment=..

#remove earlier results
rm ~/Documents/results-$experiment.zip

#collect results:
echo "collecting headers"
cd $location/$results
cat table-$experiment*.csv | head -n 7 | tail -n 1 | sort | uniq > /tmp/results-$experiment.txt
echo `cat /tmp/results-$experiment.txt | grep -o "," | wc -l` "commas, so one more columns"

echo "collecting results, removing [], and all lines that have not 137 columns"
tail -n +8 -q table-$experiment*.csv | tr -d [] | awk -F',' 'BEGIN {OFS = FS} NF=137 {print}'  >> /tmp/results-$experiment.txt

#echo "zipping results"
zip ~/Documents/results-$experiment /tmp/results-$experiment.txt
rm /tmp/results-$experiment.txt

echo "done"
