

library(magrittr)
gwasvcf::set_bcftools("/usr/local/bin/bcftools")
gwasvcf::set_plink("/usr/cluster/bin/plink")
vcf_dir <- here::here("analysis", "data", "mrcieu")
files <- list.files(vcf_dir, 
            pattern = "vcf.gz", 
            full.names = TRUE)
new_file_dir <- here::here("analysis", "data", "mrcieu_for_munge_sumstats") 
if (!dir.exists(new_file_dir)) {
    dir.create(new_file_dir)
}
for (file in files){
    new_file <- file %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        tools::file_path_sans_ext()

    file %>%
        VariantAnnotation::readVcf() %>%
        gwasglue::gwasvcf_to_TwoSampleMR(type = "outcome") %>%
        vroom::vroom_write(file = file.path(new_file_dir, paste0(new_file, ".tsv.gz")))
}

