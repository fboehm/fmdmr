library(magrittr)
# parse the command arguments
file_num <- commandArgs(trailingOnly = TRUE) %>% as.integer()

# set up for gwasvcf
gwasvcf::set_bcftools("/usr/local/bin/bcftools")
gwasvcf::set_plink("/usr/cluster/bin/plink")
# set vcf dir
vcf_dir <- here::here("analysis", "data", "mrcieu")
# get file names in vcf dir
files <- list.files(vcf_dir, 
            pattern = "vcf.gz", 
            full.names = TRUE)

# set new file directory, ie output directory
new_file_dir <- here::here("analysis", "data", "mrcieu_for_munge_sumstats") 
if (!dir.exists(new_file_dir)) {
    dir.create(new_file_dir)
}

# set file - only one file per job
file <- files[file_num]
# define new file name
new_file <- file %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    tools::file_path_sans_ext()
# convert to TwoSampleMR format & write as tsv.gz
file %>%
    VariantAnnotation::readVcf() %>%
    gwasglue::gwasvcf_to_TwoSampleMR(type = "outcome") %>%
    vroom::vroom_write(file = file.path(new_file_dir, paste0(new_file, ".tsv.gz")))

