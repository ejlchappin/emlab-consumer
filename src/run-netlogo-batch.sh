#!/bin/bash
for i in `seq 100`; 
do sbatch --qos=long --time=48:00:00 --mem=2048 run-netlogo-experiment.sh; 
done