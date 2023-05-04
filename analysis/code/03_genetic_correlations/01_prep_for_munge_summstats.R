library(magrittr)
# parse the command arguments
file_num <- commandArgs(trailingOnly = TRUE) %>% as.integer()

# set up for gwasvcf
#gwasvcf::set_bcftools("/usr/local/bin/bcftools")
#gwasvcf::set_plink("/usr/cluster/bin/plink")
# set vcf dir
download_dir <- here::here("analysis", "data", "ukb")
# get file names in vcf dir
files <- list.files(download_dir, 
            pattern = "tsv.bgz", 
            full.names = TRUE)

# set new file directory, ie output directory
new_file_dir <- here::here("analysis", "data", "ukb_for_munge_sumstats") 
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
# convert to tibble & write as tsv.gz
# note that we use column names that work with munge_summstats.py from ldsc
dd <- file %>%
    #VariantAnnotation::readVcf() %>%
    #gwasvcf::vcf_to_tibble() %>%
    vroom::vroom() 
out <- dd %>%
    dplyr::mutate(chr = purrr::map_chr(.x = variant, .f = function(x) stringr::str_split(x, pattern = ":")[[1]][1]), 
                  pos = purrr::map_chr(.x = variant, .f = function(x) stringr::str_split(x, pattern = ":")[[1]][2]),
                  ref = purrr::map_chr(.x = variant, .f = function(x) stringr::str_split(x, pattern = ":")[[1]][3]),
                  alt = purrr::map_chr(.x = variant, .f = function(x) stringr::str_split(x, pattern = ":")[[1]][4]),
                  snp = paste0(chr, ":", pos)
                  ) %>%
    dplyr::mutate(chr = as.integer(chr), pos = as.integer(pos)) %>%
    dplyr::rename(pvalue = pval, n = n_complete_samples)
# a question arises: which allele per SNP is the effect allele (ref or alt)? 
# here is the documentation to the answer: 
# https://docs.google.com/spreadsheets/d/1kvPoupSzsSFBNSztMzl04xMoSC3Kcx3CrjVf4yBmESU/edit#gid=227859291
# Variant identifier in the form "chr:pos:ref:alt", where "ref" is aligned 
# to the forward strand of GRCh37 and "alt" is the effect allele 
# (use this to join with variant annotation file).
# THE NEXT QUESTION: How do we name the effect allele for ldsc use? 
# https://github.com/bulik/ldsc/blob/aa33296abac9569a6422ee6ba7eb4b902422cc74/munge_sumstats.py#LL57C27-L57C27
# 'EFFECT_ALLELE': 'A1', 
# 'NON_EFFECT_ALLELE': 'A2'

# we will use the alt allele as A1 and the ref allele as A2


out2 <- out %>%
    dplyr::select(snp, chr, pos, ref, alt, beta, se, pvalue, n) %>%
    dplyr::rename(A1 = alt, A2 = ref) %>%
    dplyr::filter(!is.na(chr))
    
out2 %>%
    vroom::vroom_write(file = file.path(new_file_dir, paste0(new_file, ".tsv.gz")))

