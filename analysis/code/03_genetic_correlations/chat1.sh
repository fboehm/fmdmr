#!/bin/bash

# Define the path to your CSV file
CSV_FILE=~/research/fmdmr/analysis/data/manifest.csv

# Read the column names from the first row of the CSV file
IFS=',' read -r -a column_names < <(head -n 1 "$CSV_FILE")

# Find the column index for the column of interest
ldsc_column_index=-1
for i in "${!column_names[@]}"; do
    if [[ "${column_names[$i]}" = "ldsc_sumstat_file" ]]; then
        ldsc_column_index=$i
        break
    fi
done

# If the ldsc_sumstats_file column was found, extract the field from line number 5
if ((ldsc_column_index >= 0)); then
    ldsc_value=$(sed -n '2p' "$CSV_FILE" | awk -F',' -v idx=$((ldsc_column_index + 1)) '{print $idx}')
    echo "ldsc_sumstats_file value from line 2: $ldsc_value"
else
    echo "ldsc_sumstats_file column not found in the CSV file."
fi
