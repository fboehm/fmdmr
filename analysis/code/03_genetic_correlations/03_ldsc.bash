#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=10:00
#SBATCH --job-name=ldsc
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
#SBATCH --array=1-30%5
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.err




##### LD SCORE REGRESSION to get genetic correlations
LDSC=~/ldsc/ldsc_bulik/ldsc.py
data_dir=~/research/fmdmr/analysis/data/
ldsc_genetic_corr_dir=${data_dir}ldsc_genetic_correlations_diabetes_fmd/
mkdir -p ${ldsc_genetic_corr_dir}
ldsc_dir=${data_dir}ldsc/
eur_dir=${ldsc_dir}eur_w_ld_chr/
source ~/.bashrc # prep for using conda. Is this needed?
fmd_sumstats_dir=${data_dir}ldsc_fmd/
# directory where I've downloaded the "munged" sumstats from the Neale lab
neale_lab_downloads_dir=~/research/fmdmr/analysis/data/ukb_dm2_downloads_for_ldsc/

let k=0 # counter

for fmd_sumstats_file in ${fmd_sumstats_dir}*.sumstats.gz; do
    fmdstem=$(basename "$fmd_sumstats_file" .sumstats.gz)
    for file in ${neale_lab_downloads_dir}*; do
        if [[ ${file} == *.tsv.bgz ]]; then
            #filestem=$(basename "$file" .sumstats.gz)
            filestem=$(basename "$file" .tsv.bgz)
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

