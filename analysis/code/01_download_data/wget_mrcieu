#!/bin/bash

# use neale lab's phenotype manifest file (), field CA "wget"
# https://docs.google.com/spreadsheets/d/1AeeADtT0U1AukliiNyiVzVRdLYPkTbruQSk38DeutU8/edit#gid=268241601



outdir=~/research/fmdmr/analysis/data/mrcieu/
mkdir -p ${outdir}
tsv_dir=~/research/fmdmr/analysis/data/mrcieu
url_stem=https://gwas.mrcieu.ac.uk/files
# https://gwas.mrcieu.ac.uk/files/ieu-a-1102/ieu-a-1102.vcf.gz
for file in ${tsv_dir}/*; do
    if [[ ${file} == *.tsv ]]; then
        while read -r line; do
            id=$(echo ${line} | cut -f1 -d' ')
            echo ${id}
            url=${url_stem}/${id}/${id}.vcf.gz
            echo ${url}
            if [[ ! -f ${outdir}/${id}.vcf.gz ]]; then
                wget ${url} -P ${outdir}
            fi
        done < ${file}
    fi 
done



