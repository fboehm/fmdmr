#!/bin/bash

RESULTS_DIR=~/research/fmdmr/analysis/data/ldsc_genetic_correlations_diabetes_fmd/
OUT_FILE=~/research/fmdmr/analysis/results/ldsc_genetic_correlations_diabetes_fmd.txt
for file in ${RESULTS_DIR}*.log; do
    echo $file
    line=$(grep -A 1 "p1" ${file} | tail -n 1)
    echo $line
    echo $line >> ${OUT_FILE}
done 

