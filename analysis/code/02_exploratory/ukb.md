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
    knitr::kable(mr_out@Values) %>% print()
    MendelianRandomization::mr_plot(mr_in) %>% print()
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

| Method                    |   Estimate | Std Error |     95% CI |           |   P-value |
|:--------------------------|-----------:|----------:|-----------:|----------:|----------:|
| Simple median             | -0.0019065 | 0.0036449 | -0.0090503 | 0.0052374 | 0.6009414 |
| Weighted median           | -0.0003843 | 0.0036285 | -0.0074960 | 0.0067274 | 0.9156571 |
| Penalized weighted median |  0.0010226 | 0.0036018 | -0.0060368 | 0.0080821 | 0.7764763 |
| IVW                       | -0.0014697 | 0.0039077 | -0.0091286 | 0.0061892 | 0.7068343 |
| Penalized IVW             |  0.0003576 | 0.0033844 | -0.0062756 | 0.0069908 | 0.9158476 |
| Robust IVW                | -0.0008024 | 0.0037051 | -0.0080642 | 0.0064595 | 0.8285556 |
| Penalized robust IVW      |  0.0004444 | 0.0027946 | -0.0050329 | 0.0059217 | 0.8736513 |
| MR-Egger                  | -0.0001481 | 0.0176663 | -0.0347734 | 0.0344772 | 0.9933120 |
| (intercept)               | -0.0004043 | 0.0052539 | -0.0107018 | 0.0098931 | 0.9386576 |
| Penalized MR-Egger        | -0.0044071 | 0.0146638 | -0.0331475 | 0.0243334 | 0.7637646 |
| (intercept)               |  0.0015018 | 0.0044206 | -0.0071626 | 0.0101661 | 0.7340711 |
| Robust MR-Egger           | -0.0162002 | 0.0360859 | -0.0869272 | 0.0545268 | 0.6534791 |
| (intercept)               |  0.0059965 | 0.0138672 | -0.0211828 | 0.0331757 | 0.6654348 |
| Penalized robust MR-Egger | -0.0123985 | 0.0296545 | -0.0705204 | 0.0457233 | 0.6758750 |
| (intercept)               |  0.0044809 | 0.0111381 | -0.0173493 | 0.0263112 | 0.6874576 |

/net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb/30700_irnt.gwas.imputed_v3.both_sexes.varorder.tsv.bgz.1

    Rows: 13791467 Columns: 11
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): variant, minor_allele
    dbl (8): minor_AF, n_complete_samples, AC, ytx, beta, se, tstat, pval
    lgl (1): low_confidence_variant

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

| Method                    |   Estimate | Std Error |     95% CI |           |   P-value |
|:--------------------------|-----------:|----------:|-----------:|----------:|----------:|
| Simple median             | -0.0019065 | 0.0036449 | -0.0090503 | 0.0052374 | 0.6009414 |
| Weighted median           | -0.0003843 | 0.0036285 | -0.0074960 | 0.0067274 | 0.9156571 |
| Penalized weighted median |  0.0010226 | 0.0036018 | -0.0060368 | 0.0080821 | 0.7764763 |
| IVW                       | -0.0014697 | 0.0039077 | -0.0091286 | 0.0061892 | 0.7068343 |
| Penalized IVW             |  0.0003576 | 0.0033844 | -0.0062756 | 0.0069908 | 0.9158476 |
| Robust IVW                | -0.0008024 | 0.0037051 | -0.0080642 | 0.0064595 | 0.8285556 |
| Penalized robust IVW      |  0.0004444 | 0.0027946 | -0.0050329 | 0.0059217 | 0.8736513 |
| MR-Egger                  | -0.0001481 | 0.0176663 | -0.0347734 | 0.0344772 | 0.9933120 |
| (intercept)               | -0.0004043 | 0.0052539 | -0.0107018 | 0.0098931 | 0.9386576 |
| Penalized MR-Egger        | -0.0044071 | 0.0146638 | -0.0331475 | 0.0243334 | 0.7637646 |
| (intercept)               |  0.0015018 | 0.0044206 | -0.0071626 | 0.0101661 | 0.7340711 |
| Robust MR-Egger           | -0.0162002 | 0.0360859 | -0.0869272 | 0.0545268 | 0.6534791 |
| (intercept)               |  0.0059965 | 0.0138672 | -0.0211828 | 0.0331757 | 0.6654348 |
| Penalized robust MR-Egger | -0.0123985 | 0.0296545 | -0.0705204 | 0.0457233 | 0.6758750 |
| (intercept)               |  0.0044809 | 0.0111381 | -0.0173493 | 0.0263112 | 0.6874576 |

/net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb/30720_irnt.gwas.imputed_v3.both_sexes.varorder.tsv.bgz

    Rows: 13791467 Columns: 11
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): variant, minor_allele
    dbl (8): minor_AF, n_complete_samples, AC, ytx, beta, se, tstat, pval
    lgl (1): low_confidence_variant

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

| Method                    |   Estimate | Std Error |     95% CI |           |   P-value |
|:--------------------------|-----------:|----------:|-----------:|----------:|----------:|
| Simple median             |  0.0007370 | 0.0040863 | -0.0072719 | 0.0087460 | 0.8568642 |
| Weighted median           |  0.0021240 | 0.0040818 | -0.0058763 | 0.0101242 | 0.6028250 |
| Penalized weighted median |  0.0011191 | 0.0041778 | -0.0070691 | 0.0093074 | 0.7887962 |
| IVW                       |  0.0013242 | 0.0043741 | -0.0072488 | 0.0098972 | 0.7620868 |
| Penalized IVW             |  0.0025319 | 0.0031750 | -0.0036909 | 0.0087547 | 0.4251794 |
| Robust IVW                |  0.0019162 | 0.0044630 | -0.0068310 | 0.0106634 | 0.6676630 |
| Penalized robust IVW      |  0.0019187 | 0.0028783 | -0.0037226 | 0.0075600 | 0.5050088 |
| MR-Egger                  |  0.0191350 | 0.0186923 | -0.0175012 | 0.0557712 | 0.3059849 |
| (intercept)               | -0.0054488 | 0.0055590 | -0.0163443 | 0.0054467 | 0.3269995 |
| Penalized MR-Egger        |  0.0127440 | 0.0130239 | -0.0127824 | 0.0382704 | 0.3278216 |
| (intercept)               | -0.0025886 | 0.0039387 | -0.0103083 | 0.0051311 | 0.5110321 |
| Robust MR-Egger           |  0.0037655 | 0.0221761 | -0.0396989 | 0.0472300 | 0.8651671 |
| (intercept)               | -0.0003774 | 0.0055851 | -0.0113241 | 0.0105692 | 0.9461205 |
| Penalized robust MR-Egger |  0.0041369 | 0.0218401 | -0.0386689 | 0.0469428 | 0.8497642 |
| (intercept)               | -0.0006649 | 0.0053211 | -0.0110940 | 0.0097643 | 0.9005650 |

``` r
sessioninfo::session_info()
```

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.2.3 (2023-03-15)
     os       Ubuntu 18.04.6 LTS
     system   x86_64, linux-gnu
     ui       X11
     language en_US:
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       America/Detroit
     date     2023-04-11
     pandoc   1.19.2.4 @ /usr/bin/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package                * version  date (UTC) lib source
     arrangements             1.1.9    2020-09-13 [2] CRAN (R 4.0.3)
     bit                      4.0.5    2022-11-15 [1] CRAN (R 4.2.2)
     bit64                    4.0.5    2020-08-30 [2] CRAN (R 4.0.3)
     cellranger               1.1.0    2016-07-27 [2] CRAN (R 4.0.3)
     cli                      3.6.1    2023-03-23 [1] CRAN (R 4.2.3)
     codetools                0.2-19   2023-02-01 [1] CRAN (R 4.2.2)
     colorspace               2.1-0    2023-01-23 [1] CRAN (R 4.2.2)
     crayon                   1.5.2    2022-09-29 [1] CRAN (R 4.2.1)
     crosstalk                1.2.0    2021-11-04 [1] CRAN (R 4.1.2)
     data.table               1.14.8   2023-02-17 [1] CRAN (R 4.2.2)
     DBI                      1.1.3    2022-06-18 [1] CRAN (R 4.2.2)
     DEoptimR                 1.0-11   2022-04-03 [2] CRAN (R 4.2.0)
     digest                   0.6.31   2022-12-11 [1] CRAN (R 4.2.2)
     dplyr                    1.1.1    2023-03-22 [1] CRAN (R 4.2.3)
     ellipsis                 0.3.2    2021-04-29 [2] CRAN (R 4.2.1)
     evaluate                 0.20     2023-01-17 [1] CRAN (R 4.2.2)
     fansi                    1.0.4    2023-01-22 [1] CRAN (R 4.2.2)
     fastmap                  1.1.1    2023-02-24 [1] CRAN (R 4.2.3)
     foreach                  1.5.2    2022-02-02 [2] CRAN (R 4.2.0)
     generics                 0.1.3    2022-07-05 [1] CRAN (R 4.2.3)
     ggplot2                  3.4.1    2023-02-10 [1] CRAN (R 4.2.2)
     glmnet                   4.1-7    2023-03-23 [1] CRAN (R 4.2.3)
     glue                     1.6.2    2022-02-24 [1] CRAN (R 4.2.0)
     gmp                      0.7-1    2023-02-07 [1] CRAN (R 4.2.2)
     gtable                   0.3.3    2023-03-21 [1] CRAN (R 4.2.3)
     here                     1.0.1    2020-12-13 [2] CRAN (R 4.1.1)
     htmltools                0.5.5    2023-03-23 [1] CRAN (R 4.2.3)
     htmlwidgets              1.6.2    2023-03-17 [1] CRAN (R 4.2.2)
     httr                     1.4.5    2023-02-24 [1] CRAN (R 4.2.3)
     iterators                1.0.14   2022-02-05 [2] CRAN (R 4.2.0)
     iterpc                   0.4.2    2020-01-10 [2] CRAN (R 4.0.3)
     jsonlite                 1.8.4    2022-12-06 [1] CRAN (R 4.2.3)
     knitr                    1.42     2023-01-25 [1] CRAN (R 4.2.3)
     labeling                 0.4.2    2020-10-20 [2] CRAN (R 4.0.3)
     lattice                  0.20-45  2021-09-22 [2] CRAN (R 4.1.1)
     lazyeval                 0.2.2    2019-03-15 [2] CRAN (R 4.0.3)
     lifecycle                1.0.3    2022-10-07 [1] CRAN (R 4.2.2)
     magrittr               * 2.0.3    2022-03-30 [1] CRAN (R 4.2.0)
     MASS                     7.3-58.3 2023-03-07 [1] CRAN (R 4.2.3)
     Matrix                   1.5-3    2022-11-11 [1] CRAN (R 4.2.3)
     MatrixModels             0.5-1    2022-09-11 [1] CRAN (R 4.2.3)
     MendelianRandomization   0.7.0    2023-01-09 [1] CRAN (R 4.2.2)
     munsell                  0.5.0    2018-06-12 [2] CRAN (R 4.0.3)
     pillar                   1.9.0    2023-03-22 [1] CRAN (R 4.2.3)
     pkgconfig                2.0.3    2019-09-22 [2] CRAN (R 4.0.3)
     plotly                   4.10.1   2022-11-07 [1] CRAN (R 4.2.2)
     purrr                    1.0.1    2023-01-10 [1] CRAN (R 4.2.2)
     quantreg                 5.93     2022-05-02 [2] CRAN (R 4.2.0)
     R6                       2.5.1    2021-08-19 [2] CRAN (R 4.1.1)
     Rcpp                     1.0.10   2023-01-22 [1] CRAN (R 4.2.2)
     readxl                   1.4.2    2023-02-09 [1] CRAN (R 4.2.3)
     rjson                    0.2.21   2022-01-09 [2] CRAN (R 4.2.0)
     rlang                    1.1.0    2023-03-14 [1] CRAN (R 4.2.2)
     rmarkdown                2.21     2023-03-26 [1] CRAN (R 4.2.3)
     robustbase               0.95-0   2022-04-02 [2] CRAN (R 4.2.0)
     rprojroot                2.0.3    2022-04-02 [2] CRAN (R 4.2.0)
     scales                   1.2.1    2022-08-20 [1] CRAN (R 4.2.3)
     sessioninfo              1.2.2    2021-12-06 [1] CRAN (R 4.1.2)
     shape                    1.4.6    2021-05-19 [2] CRAN (R 4.1.1)
     SparseM                  1.81     2021-02-18 [2] CRAN (R 4.0.5)
     stringi                  1.7.12   2023-01-11 [1] CRAN (R 4.2.2)
     stringr                  1.5.0    2022-12-02 [1] CRAN (R 4.2.3)
     survival                 3.5-5    2023-03-12 [1] CRAN (R 4.2.3)
     tibble                   3.2.1    2023-03-20 [1] CRAN (R 4.2.3)
     tidyr                    1.3.0    2023-01-24 [1] CRAN (R 4.2.3)
     tidyselect               1.2.0    2022-10-10 [1] CRAN (R 4.2.2)
     tzdb                     0.3.0    2022-03-28 [2] CRAN (R 4.2.0)
     utf8                     1.2.3    2023-01-31 [1] CRAN (R 4.2.3)
     vctrs                    0.6.1    2023-03-22 [1] CRAN (R 4.2.3)
     viridisLite              0.4.1    2022-08-22 [1] CRAN (R 4.2.3)
     vroom                    1.6.1    2023-01-22 [1] CRAN (R 4.2.2)
     withr                    2.5.0    2022-03-03 [1] CRAN (R 4.2.0)
     xfun                     0.38     2023-03-24 [1] CRAN (R 4.2.3)
     yaml                     2.3.7    2023-01-23 [1] CRAN (R 4.2.3)

     [1] /net/mulan/home/fredboe/R/x86_64-pc-linux-gnu-library/4.0
     [2] /net/mario/cluster/lib/R/site-library-bionic-40
     [3] /usr/local/lib/R/site-library
     [4] /usr/lib/R/site-library
     [5] /usr/lib/R/library

    ──────────────────────────────────────────────────────────────────────────────
