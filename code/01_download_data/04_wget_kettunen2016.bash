# Download data from gwas catalog

mkdir -p ~/research/fmd-MR/kettunen2016/
wget http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90132001-GCST90133000/GCST90132719/GCST90132719_buildGRCh37.txt.gz -P ~/research/fmd-MR/kettunen2016/
wget --recursive --execute robots=off http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90132001-GCST90133000/GCST90132719/harmonised/ -P ~/research/fmd-MR/kettunen2016/
mkdir -p  ~/research/fmd-MR/kettunen2016/harmonised/
mv ~/research/fmd-MR/kettunen2016/ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90132001-GCST90133000/GCST90132719/harmonised/GCST90132719.tsv.gz ~/research/fmd-MR/kettunen2016/harmonised/.

## See stackexchange next time for full wget options & to avoid needing to move it after download