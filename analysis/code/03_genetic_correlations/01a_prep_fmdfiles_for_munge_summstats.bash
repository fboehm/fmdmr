#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=1:00:00
#SBATCH --job-name=prep_fmdfiles_for_munge_summstats
#SBATCH --mem=64G
#SBATCH --cpus-per-task=1
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/prep_fmdfiles_for_munge_summstats_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/prep_fmdfiles_for_munge_summstats_%a.err


if [ -n $SLURM_JOB_ID ] ; then
    FILE_PATH=$(scontrol show job $SLURM_JOBID | awk -F= '/Command=/{print $2}')
else
    FILE_PATH=$(realpath $0)
fi

SCRIPT_DIR="$(dirname "$FILE_PATH")"
echo $SCRIPT_DIR
CODE_DIR="$(dirname "$SCRIPT_DIR")"

echo $CODE_DIR
PROJECT_DIR="$(dirname "$CODE_DIR")"
echo $PROJECT_DIR

ulimit -s unlimited

Rscript ${SCRIPT_DIR}/01a_prep_fmdfiles_for_munge_summstats.R 
