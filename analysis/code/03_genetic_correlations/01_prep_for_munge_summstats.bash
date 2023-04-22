#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=1:00:00
#SBATCH --job-name=prep_for_munge_summstats
#SBATCH --mem=64G
#SBATCH --cpus-per-task=1
#SBATCH --array=39
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/prep_for_munge_summstats_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/prep_for_munge_summstats_%a.err


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

let k=0

for file_num in `seq 1 39`; do
    let k=${k}+1
    if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
        echo "Processing file number ${file_num}"
        Rscript ${SCRIPT_DIR}/01_prep_for_munge_summstats.R ${file_num} 
    fi
done
