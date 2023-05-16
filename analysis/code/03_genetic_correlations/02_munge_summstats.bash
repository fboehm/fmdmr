#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=10-00:00:00
#SBATCH --job-name=munge_sumstats
#SBATCH --mem=64G
#SBATCH --array=1-9
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/munge_sumstats_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/munge_sumstats_%a.err


# https://github.com/bulik/ldsc/wiki/Heritability-and-Genetic-Correlation
# we'll download ld scores for Europeans

ldsc_dir=~/research/fmdmr/analysis/data/ldsc/
mkdir -p ${ldsc_dir}

# call munge_sumstats.py
MUNGE_SUMSTATS=~/ldsc/munge_sumstats.py
PATH_TO_GWAS_FILES=~/research/fmdmr/analysis/data/ukb_for_munge_sumstats/
FILENAME_ARRAY=( $(ls ${PATH_TO_GWAS_FILES}*.tsv.gz) )

let k=0

for file in ${FILENAME_ARRAY[@]}; do
    let k=${k}+1
    filestem=$(basename "$file" .tsv.gz)
    # munge here!
    if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
        if [[ ! -f ${ldsc_dir}${filestem}.sumstats.gz ]]; then
            echo "munging ${file}"
            conda run -n ldsc python ${MUNGE_SUMSTATS} \
                --sumstats ${file} \
                --out ${ldsc_dir}${filestem} \
                --merge-alleles ${ldsc_dir}LDSCORE_w_hm3.snplist
        fi
    fi
done

