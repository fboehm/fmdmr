#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=1-00:00:00
#SBATCH --job-name=ldsc
#SBATCH --mem=64G
#SBATCH --cpus-per-task=1
#SBATCH --array=1-78%20
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
    echo ${fmdstem}
    for file in ${ldsc_dir}*; do
        if [[ ${file} == *.sumstats.gz ]]; then
            filestem=$(basename "$file" .sumstats.gz)
            echo "file is ${file} and fmd_sumstats_file is ${fmd_sumstats_file}\n"
            let k=${k}+1
            if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
                conda run -n ldsc python ${LDSC} \
                    --rg ${file},${fmd_sumstats_file} \
                    --ref-ld-chr ${eur_dir} \
                    --w-ld-chr ${eur_dir} \
                    --out ${ldsc_genetic_corr_dir}${filestem}_${fmdstem}
            fi
        fi 
    done
done

