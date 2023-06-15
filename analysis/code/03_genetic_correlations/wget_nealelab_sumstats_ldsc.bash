#!/bin/bash

#SBATCH --job-name=wget_nealelab_sumstats_ldsc
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.err
#SBATCH --time=35:00
#SBATCH --mem=200MB
#SBATCH --partition=mulan
#SBATCH --array=1-12099%400

DATA_DIR=~/research/fmdmr/analysis/data/
CSV_FILE=${DATA_DIR}manifest2.csv
OUT_DIR=${DATA_DIR}sumstats_nealelab2/
echo "OUT_DIR: ${OUT_DIR}"
# Define the output directory
echo "SLURM_ARRAY_TASK_ID: ${SLURM_ARRAY_TASK_ID}"


# Determine columns to get by name/number
# Read the column names from the first row of the CSV file

get_column_index() { # first arg must be column name, second arg must be csv file with full path
    local column_name="$1"
    local csv_file="$2"
    local column_index=-1
    IFS=',' read -r -a column_names < <(head -n 1 "$csv_file")
    for i in "${!column_names[@]}"; do
        if [[ "${column_names[$i]}" = "$column_name" ]]; then
            column_index=$i
            break
        fi
    done
    echo "$column_index"
}

get_column_value() {
    local column_index="$1"
    local csv_file="$2"
    local line_number="$3"
    local col_value=$(sed -n "${line_number}p" "$csv_file" | awk -F',' -v idx=$((column_index + 1)) '{print $idx}')
    echo "$col_value"
}

# determine column index for ldsc_sumstats_file
ldsc_sf_index=$(get_column_index "ldsc_sumstat_file" "$CSV_FILE")
echo "ldsc_sf_index: $ldsc_sf_index"

# If the ldsc_sumstats_file column was found, extract the field from line number 5
ldsc_sf_value=$(get_column_value "$ldsc_sf_index" "$CSV_FILE" ${SLURM_ARRAY_TASK_ID})
echo "ldsc_sf_value from line ${SLURM_ARRAY_TASK_ID}: $ldsc_sf_value"

######
# determine column index for ldsc_sumstat_dropbox
ldsc_sd_index=$(get_column_index "ldsc_sumstat_dropbox" "$CSV_FILE")
echo "ldsc_sd_index: $ldsc_sd_index"

# If the ldsc_sumstats_file column was found, extract the field from line number 5
ldsc_sd_value=$(get_column_value "$ldsc_sd_index" "$CSV_FILE" ${SLURM_ARRAY_TASK_ID})
echo "ldsc_sd_value from line ${SLURM_ARRAY_TASK_ID}: $ldsc_sd_value"

outfile=${OUT_DIR}${ldsc_sf_value}
echo "outfile: $outfile"
# Read the command from the "command" column in the CSV file based on the array task ID
#wget_command=$(awk -F',' -v line=${SLURM_ARRAY_TASK_ID} 'NR==line{print $8}' ${CSV_FILE})
#echo "wget_command is: $wget_command"
mkdir -p $OUT_DIR

if [ ! -f "${outfile}" ]; then
    if [ ! -z "${ldsc_sd_value}" ]; then
        wget_command="wget -O ${outfile} ${ldsc_sd_value}"
        echo "wget_command is: $wget_command"
        eval "$wget_command"
    else 
        echo "ldsc_sd_value is empty"
    fi
fi
