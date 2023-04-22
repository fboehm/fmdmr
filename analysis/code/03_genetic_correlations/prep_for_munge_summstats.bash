#!/bin/bash

DOWNLOAD_DIR=~/research/fmdmr/analysis/data/mrcieu/
NUM_FILES=$(ls -1 ${DOWNLOAD_DIR}*.vcf.gz | wc -l)

#SBATCH --partition=mulan,main
#SBATCH --time=1-00:00:00
#SBATCH --job-name=prep_for_munge_summstats
#SBATCH --mem=8G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-${NUM_FILES}%40
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/cluster_outputs/prep_for_munge_summstats_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/cluster_outputs/prep_for_munge_summstats_%a.err


CLUSTER_OUTPUTS_DIR=~/research/fmdmr/cluster_outputs/
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


let k=0

for file_num in `seq 1 ${NUM_FILES}`; do
    let k=${k}+1
    if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
        echo "Processing file number ${}"
        Rscript ${SCRIPT_DIR}/prep_for_munge_summstats.R ${file_num} 
    fi
done
