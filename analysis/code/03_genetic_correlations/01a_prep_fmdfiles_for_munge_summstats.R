library(magrittr)
files <- c(here::here("analysis", "data", "fmd", "GCST90026612_buildGRCh37.tsv"),
           here::here("analysis", "data", "fmd_meta_gwas", "meta_analyse_FMD_FUMA_FR_MAYO_DEFINE_POL_MGI_FEIRI_HRC_all_2020-08-12.tab")
        )
# set new file directory, ie output directory
new_file_dir <- here::here("analysis", "data", "fmd_for_munge_sumstats") 
if (!dir.exists(new_file_dir)) {
    dir.create(new_file_dir)
}
for (file_num in 1:2){

    file <- files[file_num]
    # define new file name
    new_file <- file %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        tools::file_path_sans_ext()
    # convert to tibble & write as tsv.gz
    # note that we use column names that work with munge_summstats.py from ldsc
    dat <- file %>%
        vroom::vroom(col_types = vroom::cols())
    if (file_num == 1){
        dat %>%
            dplyr::rename(position = base_pair_location,
                        a1 = EA,
                        a2 = OA) %>%
            dplyr::select(-c(EAF, Het_P)) %>%
            dplyr::mutate(N = N_cases + N_ctrls) %>%
            dplyr::select(-c(N_cases, N_ctrls)) %>%
            vroom::vroom_write(file = file.path(new_file_dir, paste0(new_file, ".tsv.gz")))
    }
    if (file_num == 2){
        dat %>%
            dplyr::rename(position = POS,
                        a1 = REF,
                        a2 = ALT,
                        chromosome = CHROM) %>%
            dplyr::select(-c(Rsq_min, Rsq_max, MarkerName)) %>%
            vroom::vroom_write(file = file.path(new_file_dir, paste0(new_file, ".tsv.gz")))
    }
}