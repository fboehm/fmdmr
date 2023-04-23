#!/bin/bash

#SBATCH --partition=mulan,main
#SBATCH --time=2-00:00:00
#SBATCH --job-name=munge_fmd
#SBATCH --mem=64G
#SBATCH --array=1-2
#SBATCH --output=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/munge_fmd_%a.out
#SBATCH --error=/net/mulan/home/fredboe/research/fmdmr/analysis/cluster_outputs/munge_fmd_%a.err


# https://github.com/bulik/ldsc/wiki/Heritability-and-Genetic-Correlation
# we'll download ld scores for Europeans

eur_dir=~/research/fmdmr/analysis/data/ldsc/eur_w_ld_chr/

#if [ -d "$eur_dir" ]; then
#    wget https://data.broadinstitute.org/alkesgroup/LDSCORE/eur_w_ld_chr.tar.bz2 -P ${ldsc_dir}
#    tar -jxvf ${ldsc_dir}eur_w_ld_chr.tar.bz2
#fi
# This will create a new directory in your current working directory named eur_w_ld_chr/. 
#### FORMAT GWAS FILES
#We strongly recommend that you use the script munge_sumstats.py included in this github repository in order to convert summary statistics into the ldsc format, because this script checks for a lot of annoying gotchas that have gotten us in trouble before.
#
#The ldsc .sumstats format requires six pieces of information for each SNP:
#
#A unique identifier (e.g., the rs number)
#Allele 1 (effect allele)
#Allele 2 (non-effect allele)
#Sample size (which often varies from SNP to SNP)
#A P-value
#A signed summary statistic (beta, OR, log odds, Z-score, etc)

### Note on imputation from github wiki:

# Imputation quality is correlated with LD Score, and low imputation 
# quality yields lower test statistics, so imputation quality is a 
# confounder for LD Score regression. To prevent bias from variable 
# imputation quality, we usually remove poorly-imputed SNPs by 
# filtering on INFO > 0.9. The scz and bip summary statistics that 
# we're using for this tutorial have INFO columns, so munge_sumstats.py 
# will automatically perform the filtering. If you're using summary 
# statistics that don't come with an INFO column, we recommend filtering 
# to HapMap3 SNPs (using the --merge or --merge-alleles flags), 
# because these seem to be well-imputed in most studies.

# It is a good idea to check that the alleles listed in your summary statistics files match the alleles listed in the data used to estimate LD Scores. Sometimes a small number of alleles won't match; this usually indicates mis-labeled SNPs. This is accomplished using the --merge-alleles flag which takes as its argument a file with a list of SNPs and alleles. You can download the required alleles file with the following command (or by manually following the download link if your machine does not have the wget utility):

#if [[ ! -f ${ldsc_dir}w_hm3.snplist ]]; then
#    wget https://data.broadinstitute.org/alkesgroup/LDSCORE/w_hm3.snplist.bz2 -P ${ldsc_dir}
#    bunzip2 ${ldsc_dir}w_hm3.snplist.bz2
#fi

# To convert the summary statistics, type the commands

#munge_sumstats.py \
#--sumstats pgc.cross.SCZ17.2013-05.txt \
#--N 17115 \
#--out scz \
#--merge-alleles w_hm3.snplist

#munge_sumstats.py \
#--sumstats pgc.cross.BIP11.2013-05.txt \
#--N 11810 \
#--out bip \
#--merge-alleles w_hm3.snplist
#These commands should take about 20 seconds each, though of course the precise time will vary from machine to machine. This will print a series of log messages to the terminal (described below), along with files, scz.log, scz.sumstats.gz and bip.log, bip.sumstats.gz. munge_sumstats.py will print warning messages labeled WARNING to the log file if it finds anything troubling. You can and should search your log files for warnings with the command grep 'WARNING' *log. It turns out there are no warnings for these data.
#
#Note that munge_sumstats.py interprets A1 as the reference allele and that the A1 column in the .sumstats file format refers to the reference allele.

# NEED TO RUN THIS BEFORE USING ldsc:

# conda activate ldsc

## THEN, after this script, run:

# conda deactivate 

source ~/.bashrc # prep for using conda. Is this needed?
#conda activate ldsc
ldsc_dir=~/research/fmdmr/analysis/data/ldsc/


# call munge_sumstats.py
MUNGE_SUMSTATS=~/ldsc/munge_sumstats.py
##### Munge fmd files

let k=0

fmd_sumstats_dir=~/research/fmdmr/analysis/data/ldsc_fmd/
mkdir -p ${fmd_sumstats_dir}
PATH_TO_GWAS_FILES=~/research/fmdmr/analysis/data/fmd_for_munge_sumstats/
FILENAME_ARRAY=( $(ls ${PATH_TO_GWAS_FILES}*.tsv.gz) )

for fmdfile in ${FILENAME_ARRAY[@]}; do
    let k=${k}+1
    fmdfilestem=$(basename "$fmdfile" .tsv.gz )
    
    if [ ${k} -eq ${SLURM_ARRAY_TASK_ID} ]; then
        echo "munging ${fmdfilestem}"
        conda run -n ldsc python ${MUNGE_SUMSTATS} \
            --sumstats ${fmdfile} \
            --out ${fmd_sumstats_dir}${fmdfilestem} \
            --merge-alleles ${ldsc_dir}LDSCORE_w_hm3.snplist
    fi
done







