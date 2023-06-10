#!/bin/bash

#SBATCH --job-name=wget_nealelab_sumstats_ldsc
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/wget_nealelab_sumstats_ldsc_%a.err
#SBATCH --time=1:00:00
#SBATCH --mem=100MB
#SBATCH --partition=mulan
#SBATCH --array=2-12098%200

DATA_DIR=~/research/fmdmr/analysis/data/
CSV_FILE=${DATA_DIR}manifest.csv
#OUT_DIR=${DATA_DIR}sumstats_nealelab/
# Define the output directory

# Read the command from the "command" column in the CSV file based on the array task ID
command=$(awk -F',' -v line=${SLURM_ARRAY_TASK_ID} 'NR==line{print $8}' ${CSV_FILE})

echo "command is: $command"

# Execute the command
eval "$command"
