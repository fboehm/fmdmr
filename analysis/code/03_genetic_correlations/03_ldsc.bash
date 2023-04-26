#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=10:00:00
#SBATCH --job-name=ldsc
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
#SBATCH --array=1-14
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.err




##### LD SCORE REGRESSION to get genetic correlations
LDSC=~/ldsc/ldsc.py
ldsc_genetic_corr_dir=~/research/fmdmr/analysis/data/ldsc_genetic_correlations/
mkdir -p ${ldsc_genetic_corr_dir}
ldsc_dir=~/research/fmdmr/analysis/data/ldsc/
eur_dir=~/research/fmdmr/analysis/data/ldsc/eur_w_ld_chr/
source ~/.bashrc # prep for using conda. Is this needed?
fmd_sumstats_dir=~/research/fmdmr/analysis/data/ldsc_fmd/

let k=0 # counter

for fmd_sumstats_file in ${fmd_sumstats_dir}*.sumstats.gz; do
    fmdstem=$(basename "$fmd_sumstats_file" .sumstats.gz)
    for file in ${ldsc_dir}*; do
        if [[ ${file} == *.sumstats.gz ]]; then
            filestem=$(basename "$file" .sumstats.gz)
            let k=${k}+1
            if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
                #echo "file is ${file} and fmd_sumstats_file is ${fmd_sumstats_file}\n"
                conda run -n ldsc python ${LDSC} \
                    --rg ${file},${fmd_sumstats_file} \
                    --ref-ld-chr ${eur_dir} \
                    --w-ld-chr ${eur_dir} \
                    --out ${ldsc_genetic_corr_dir}${filestem}_${fmdstem}
            fi
        fi 
    done
done

