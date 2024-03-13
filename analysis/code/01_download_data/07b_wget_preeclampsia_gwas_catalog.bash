
mydir=../../data/preeclampsia_gwas_catalog/
mkdir -p ${mydir}
dir_remote=http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90269001-GCST90270000/GCST90269903/

wget --recursive --no-parent -e robots=off ${dir_remote} -P ${mydir}

dir_remote=http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90269001-GCST90270000/GCST90269904/
wget --recursive --no-parent -e robots=off ${dir_remote} -P ${mydir}


dir_remote=http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90269001-GCST90270000/GCST90269905/
wget --recursive --no-parent -e robots=off ${dir_remote} -P ${mydir}
