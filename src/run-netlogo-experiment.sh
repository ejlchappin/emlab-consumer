#!/bin/bash
location=..
experiment=..
netlogo=..
model=model.nlogo
$netlogo/netlogo-headless.sh --threads 1 --model $location/$model --experiment $experiment --table $location/results/table-$experiment-$SLURM_JOBID.csv -DXmx=2048m
