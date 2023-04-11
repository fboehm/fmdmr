UKB creatinine & Cystatin C
================

``` r
library(magrittr)
```

``` r
fmd_file <- here::here("analysis", "data", "fmd", "GCST90026612_buildGRCh37.tsv")
fmd <- vroom::vroom(fmd_file) %>%
    dplyr::mutate(chr_pos = paste0("chr", chromosome, ":", base_pair_location))
```

    Rows: 5483710 Columns: 12
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (3): SNP, OA, EA
    dbl (9): chromosome, base_pair_location, EAF, BETA, SE, p_value, Het_P, N_ca...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fmd_lead_snps_file <- here::here("analysis", "data", "fmd", "41467_2021_26174_MOESM4_ESM.xlsx") # snps from Katz et al. 2022 Supp Info Table SII.
```

``` r
files <- list.files(here::here("analysis", "data", "ukb"), pattern = "both_sexes", full.names = TRUE)
# see neale lab documentation: https://docs.google.com/spreadsheets/d/1kvPoupSzsSFBNSztMzl04xMoSC3Kcx3CrjVf4yBmESU/edit#gid=227859291
# GRCh37 is used here!
for (file in files) {
    cat(file, "\n")
    pre_dat <- vroom::vroom(file)
    fmd_lead_snps <- readxl::read_xlsx(fmd_lead_snps_file, skip = 1, col_names = TRUE) %>%
    dplyr::filter(`P-value` < 10^-6) %>%
    dplyr::mutate(chr_pos = paste0("chr", CHROM, ":", POS)) %>%
    dplyr::rename(other_allele = 4, effect_allele = 5, gene = 6, pvalue = 10) %>%
    dplyr::mutate(v1 = paste0(CHROM, ":", POS, ":", other_allele, ":", effect_allele)) %>%
    dplyr::mutate(v2 = paste0(CHROM, ":", POS, ":", effect_allele, ":", other_allele)) %>%
    dplyr::mutate(v1_in_dat = v1 %in% pre_dat$variant) %>%
    dplyr::mutate(v2_in_dat = v2 %in% pre_dat$variant)

    dat <- pre_dat %>%
    # filter to chromosome values in fmd_lead_snps
    #    dplyr::filter(stringr::str_starts(string = variant, pattern = "12") | 
    #                  stringr::str_starts(string = variant, pattern = "6") |
    #                  stringr::str_starts(string = variant, pattern = "5") |
    #                  stringr::str_starts(string = variant, pattern = "8") | 
    #                  stringr::str_starts(string = variant, pattern = "13") |
    #                  stringr::str_starts(string = variant, pattern = "4") |
    #                  stringr::str_starts(string = variant, pattern = "10") | 
    #                  stringr::str_starts(string = variant, pattern = "17")
    #                   ) %>%
        dplyr::filter(variant %in% fmd_lead_snps$v1 | variant %in% fmd_lead_snps$v2) %>%
        dplyr::mutate(chr = purrr::map_int(.x = variant, .f = function(x) {stringr::str_split(string = x, pattern = ":")[[1]][1] %>% as.integer})) %>%
        dplyr::mutate(pos = purrr::map_int(.x = variant, .f = function(x) {stringr::str_split(string = x, pattern = ":")[[1]][2] %>% as.integer})) %>%
        dplyr::mutate(ref = purrr::map_chr(.x = variant, .f = function(x) {stringr::str_split(string = x, pattern = ":")[[1]][3]})) %>%
        dplyr::mutate(alt = purrr::map_chr(.x = variant, .f = function(x) {stringr::str_split(string = x, pattern = ":")[[1]][4]})) %>% 
        dplyr::mutate(chr_pos = paste0("chr", chr, ":", pos)) %>%
        dplyr::relocate(chr, pos, ref, alt, chr_pos)
    fmd_lead_snps$chr_pos %in% dat$chr_pos

    #Neale lab denotes by beta the alt allele effect, ie, the increase in phenotype per increase in one alt allele.

    #Are any of the snps shared between ukb and fmd?

    (fmd$chr_pos %in% dat$chr_pos) %>% sum()
    # both are on GRCh37
    # join fmd_lead_snps and dat - where dat has data for only 10 snp
    combined <- fmd_lead_snps %>% 
        dplyr::rename_with(~ paste0("fmd_", .), .cols = c(- chr_pos, -v1, -v2, - v1_in_dat, - v2_in_dat)) %>%
        dplyr::left_join(dat, by = "chr_pos")
    combined %>%
        dplyr::relocate(v1, v2, v1_in_dat, v2_in_dat, fmd_effect_allele, ref)
    comb2 <- combined %>%
        dplyr::mutate(harmonised_dat_effect_allele = ifelse(fmd_effect_allele == alt, alt, ref)) %>%
        dplyr::mutate(harmonised_dat_effect = ifelse(fmd_effect_allele == alt, beta, -beta))
    mr_in <- MendelianRandomization::mr_input(bx = comb2$fmd_BETA, bxse = comb2$fmd_SE, by = comb2$harmonised_dat_effect, byse = comb2$se)
    mr_out <- MendelianRandomization::mr_allmethods(mr_in)
    knitr::kable(mr_out@Values)
    MendelianRandomization::mr_plot(mr_in)
}
```

    /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb/30700_irnt.gwas.imputed_v3.both_sexes.varorder.tsv.bgz 

    Rows: 13791467 Columns: 11
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): variant, minor_allele
    dbl (8): minor_AF, n_complete_samples, AC, ytx, beta, se, tstat, pval
    lgl (1): low_confidence_variant

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb/30700_irnt.gwas.imputed_v3.both_sexes.varorder.tsv.bgz.1 

    Rows: 13791467 Columns: 11
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): variant, minor_allele
    dbl (8): minor_AF, n_complete_samples, AC, ytx, beta, se, tstat, pval
    lgl (1): low_confidence_variant

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb/30720_irnt.gwas.imputed_v3.both_sexes.varorder.tsv.bgz 

    Rows: 13791467 Columns: 11
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): variant, minor_allele
    dbl (8): minor_AF, n_complete_samples, AC, ytx, beta, se, tstat, pval
    lgl (1): low_confidence_variant

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
