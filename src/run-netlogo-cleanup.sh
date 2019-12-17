#!/bin/bash
location=..
results=results

#clean all hpc results:
echo "cleaning all hpc results files..."
rm $location/$results/*
rm $location/slurm*
rm /tmp/*
echo "done"
