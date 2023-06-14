#!/bin/bash

#SBATCH --job-name=wget_nealelab_sumstats_ldsc
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.err
#SBATCH --time=14:00:00
#SBATCH --mem=200MB
#SBATCH --partition=mulan
#SBATCH --array=2

DATA_DIR=~/research/fmdmr/analysis/data/
CSV_FILE=${DATA_DIR}manifest.csv
OUT_DIR=${DATA_DIR}sumstats_nealelab/
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
ldsc_index=$(get_column_index "ldsc_sumstat_file" "$CSV_FILE")
echo "ldsc_index: $ldsc_index"

# If the ldsc_sumstats_file column was found, extract the field from line number 5
if ($ldsc_index >= 0); then
    ldsc_value=$(get_column_value "$ldsc_index" "$CSV_FILE" ${SLURM_ARRAY_TASK_ID})
    echo "ldsc_value from line ${SLURM_ARRAY_TASK_ID}: $ldsc_value"
else
    echo "ldsc_sumstat_file column not found in the CSV file, $CSV_FILE"
fi
ldsc_sumstat_file=$ldsc_value
echo "ldsc_sumstat_file: $ldsc_sumstat_file"

######
# determine column index for ldsc_sumstat_dropbox
ldsc_index=$(get_column_index "ldsc_sumstat_dropbox" "$CSV_FILE")
echo "ldsc_index: $ldsc_index"

# If the ldsc_sumstats_file column was found, extract the field from line number 5
if ($ldsc_index >= 0); then
    ldsc_value=$(get_column_value "$ldsc_index" "$CSV_FILE" ${SLURM_ARRAY_TASK_ID})
    echo "ldsc_value from line ${SLURM_ARRAY_TASK_ID}: $ldsc_value"
else
    echo "ldsc_sumstat_dropbox column not found in the CSV file, $CSV_FILE"
fi
ldsc_sumstat_dropbox=$ldsc_value
echo "ldsc_sumstat_dropbox: $ldsc_sumstat_dropbox"

outfile=${OUT_DIR}${ldsc_sumstat_file}
echo "outfile: $outfile"
# Read the command from the "command" column in the CSV file based on the array task ID
#wget_command=$(awk -F',' -v line=${SLURM_ARRAY_TASK_ID} 'NR==line{print $8}' ${CSV_FILE})
#echo "wget_command is: $wget_command"
if [ ! -d "$OUT_DIR" ]; then
  mkdir $OUT_DIR
fi

if [ ! -f "${outfile}" ]; then
    if [ ! -z "${ldsc_sumstat_dropbox}" ]; then
        wget_command="wget -O ${outfile} ${ldsc_sumstat_dropbox}"
        echo "wget_command is: $wget_command"
        eval "$wget_command"
    else 
        echo "ldsc_sumstat_dropbox is empty"
    fi
fi
