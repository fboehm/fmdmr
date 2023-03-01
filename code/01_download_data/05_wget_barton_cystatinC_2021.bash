# Download data from gwas catalog

mydir=~/research/fmd-MR/barton2021/
mkdir -p ${mydir}
wget http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90025001-GCST90026000/GCST90025945/GCST90025945_buildGRCh37.tsv -P ${mydir}

