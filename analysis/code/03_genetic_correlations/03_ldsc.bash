#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=3:00:00
#SBATCH --job-name=ldsc
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --array=1-12098%300
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/ldsc_%a.err




##### LD SCORE REGRESSION to get genetic correlations
LDSC=~/ldsc/ldsc_bulik/ldsc.py
data_dir=~/research/fmdmr/analysis/data/
ldsc_genetic_corr_dir=${data_dir}ldsc_genetic_correlations_fmd2/
mkdir -p ${ldsc_genetic_corr_dir}
ldsc_dir=${data_dir}ldsc/
eur_dir=${ldsc_dir}eur_w_ld_chr/
source ~/.bashrc # prep for using conda. Is this needed?
fmd_sumstats_dir=${data_dir}ldsc_fmd/
# directory where I've downloaded the "munged" sumstats from the Neale lab
#neale_lab_downloads_dir=~/research/fmdmr/analysis/data/ukb_dm2_downloads_for_ldsc/
neale_lab_downloads_dir=${data_dir}sumstats_nealelab2/

let k=0 # counter

#for fmd_sumstats_file in ${fmd_sumstats_dir}*.sumstats.gz; do
    fmd_sumstats_file=${fmd_sumstats_dir}GCST90026612_buildGRCh37.sumstats.gz
    fmdstem=$(basename "$fmd_sumstats_file" .sumstats.gz)
    for file in ${neale_lab_downloads_dir}*; do
        if [[ ${file} == *.tsv.* ]]; then
            #filestem=$(basename "$file" .sumstats.gz)
            filestem=$(basename "$file" .tsv.gz)
            filestem=$(basename "$filestem" .tsv.bgz)
            outfile=${ldsc_genetic_corr_dir}${filestem}_${fmdstem}
            let k=${k}+1
            if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
                #echo "file is ${file} and fmd_sumstats_file is ${fmd_sumstats_file}\n"
                if [ ! -f ${outfile}.log ]; then
                    conda run -n ldsc python ${LDSC} \
                        --rg ${file},${fmd_sumstats_file} \
                        --ref-ld-chr ${eur_dir} \
                        --w-ld-chr ${eur_dir} \
                        --out ${outfile} 
                fi    
            fi
        fi 
    done
#done

