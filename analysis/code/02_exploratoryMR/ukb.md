Creatinine & Cystatin C & CKD as possible consequences of FMD
================

``` r
library(magrittr)
```

``` r
fmd_file <- here::here("analysis", "data", "fmd", "GCST90026612_buildGRCh37.tsv")
fmd <- TwoSampleMR::read_exposure_data(filename = fmd_file,
                                        sep = "\t",
                                        snp_col = "SNP",
                                        beta_col = "BETA",
                                        effect_allele_col = "EA",
                                        other_allele_col = "OA",
                                        eaf_col = "EAF",
                                        se_col = "SE",
                                        pval_col = "p_value",
                                        chr_col = "chromosome",
                                        pos_col = "base_pair_location",
                                        ncase_col = "N_cases",
                                        ncontrol_col = "N_ctrls"
                                        )
```

    No phenotype name specified, defaulting to 'exposure'.

    Generating sample size from ncase and ncontrol

``` r
pvalue_thresholds <- c(1e-8, 1e-7, 1e-6)
```

``` r
for (pvalue_threshold in pvalue_thresholds){
    fmd2 <- fmd %>% 
        dplyr::filter(pval.exposure < pvalue_threshold) %>%
        TwoSampleMR::clump_data()
    files <- list.files(here::here("analysis", "data", "ukb_for_munge_sumstats"), pattern = "female", full.names = TRUE)
    for (file in files){
        pre_dat <- vroom::vroom(file, col_types = "ciiccdddi")
        pre_dat <- pre_dat %>%
            dplyr::filter((chr %in% fmd2$chr.exposure) & (pos %in% fmd2$pos.exposure))
        pre2 <- pre_dat %>%
            dplyr::left_join(fmd2 %>% dplyr::select(chr.exposure, pos.exposure, SNP), by = c("chr" = "chr.exposure", "pos" = "pos.exposure")) %>%
            dplyr::select(- snp) %>%
            TwoSampleMR::format_data(type = "outcome", 
                                    effect_allele_col = "A1", 
                                    other_allele_col = "A2",
                                    pval_col = "pvalue",
                                    samplesize_col = "n"       
                                    )
        dat_harm <- TwoSampleMR::harmonise_data(exposure_dat = fmd2, outcome_dat = pre2)
        result <- TwoSampleMR::mr(dat_harm) 
        # omit method that gives error
        cat("## ", file, "\n")
        cat("### pvalue threshold: ", pvalue_threshold, "\n")
        knitr::knit_print(knitr::kable(result))
    }
}
```

    API: public: http://gwas-api.mrcieu.ac.uk/

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping 3U3qWI, 7 variants, using EUR population reference

    Removing 4 of 7 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (rTUcnL)

    Analysing '3U3qWI' on 'rTUcnL'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (5k2Wed)

    Analysing '3U3qWI' on '5k2Wed'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (f1VVrK)

    Analysing '3U3qWI' on 'f1VVrK'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-08

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping 3U3qWI, 27 variants, using EUR population reference

    Removing 23 of 27 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (1TZbTL)

    Analysing '3U3qWI' on '1TZbTL'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (n3I8SG)

    Analysing '3U3qWI' on 'n3I8SG'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (EVkNQa)

    Analysing '3U3qWI' on 'EVkNQa'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-07

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping 3U3qWI, 88 variants, using EUR population reference

    Removing 78 of 88 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (83ZFbA)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing '3U3qWI' on '83ZFbA'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (7dqXEa)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing '3U3qWI' on '7dqXEa'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (3U3qWI) and outcome (PPwNe8)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing '3U3qWI' on 'PPwNe8'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-06

``` r
# see neale lab documentation: https://docs.google.com/spreadsheets/d/1kvPoupSzsSFBNSztMzl04xMoSC3Kcx3CrjVf4yBmESU/edit#gid=227859291
# GRCh37 is used here!
#Neale lab denotes by beta the alt allele effect, ie, the increase in phenotype per increase in one alt allele.
```

``` r
sessioninfo::session_info()
```

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.3.0 (2023-04-21)
     os       Ubuntu 18.04.6 LTS
     system   x86_64, linux-gnu
     ui       X11
     language en_US:
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       America/Detroit
     date     2023-05-12
     pandoc   1.19.2.4 @ /usr/bin/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package     * version date (UTC) lib source
     bit           4.0.5   2022-11-15 [1] CRAN (R 4.2.2)
     bit64         4.0.5   2020-08-30 [2] CRAN (R 4.0.3)
     cli           3.6.1   2023-03-23 [1] CRAN (R 4.2.3)
     codetools     0.2-19  2023-02-01 [1] CRAN (R 4.2.2)
     colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.2.2)
     crayon        1.5.2   2022-09-29 [1] CRAN (R 4.2.1)
     crul          1.3     2022-09-03 [1] CRAN (R 4.2.3)
     curl          5.0.0   2023-01-12 [1] CRAN (R 4.2.2)
     data.table    1.14.8  2023-02-17 [1] CRAN (R 4.2.2)
     digest        0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
     dplyr         1.1.1   2023-03-22 [1] CRAN (R 4.2.3)
     evaluate      0.20    2023-01-17 [1] CRAN (R 4.2.2)
     fansi         1.0.4   2023-01-22 [1] CRAN (R 4.2.2)
     fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.2.3)
     foreach       1.5.2   2022-02-02 [2] CRAN (R 4.2.0)
     generics      0.1.3   2022-07-05 [1] CRAN (R 4.2.3)
     ggplot2       3.4.2   2023-04-03 [1] CRAN (R 4.2.3)
     ggrepel       0.9.3   2023-02-03 [1] CRAN (R 4.2.3)
     glmnet        4.1-7   2023-03-23 [1] CRAN (R 4.2.3)
     glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
     gridExtra     2.3     2017-09-09 [2] CRAN (R 4.0.3)
     gtable        0.3.3   2023-03-21 [1] CRAN (R 4.2.3)
     here          1.0.1   2020-12-13 [2] CRAN (R 4.1.1)
     htmltools     0.5.5   2023-03-23 [1] CRAN (R 4.2.3)
     httpcode      0.3.0   2020-04-10 [1] CRAN (R 4.2.2)
     httr          1.4.5   2023-02-24 [1] CRAN (R 4.2.3)
     ieugwasr      0.1.5   2023-04-13 [1] Github (mrcieu/ieugwasr@33e4629)
     iterators     1.0.14  2022-02-05 [2] CRAN (R 4.2.0)
     jsonlite      1.8.4   2022-12-06 [1] CRAN (R 4.2.3)
     knitr         1.42    2023-01-25 [1] CRAN (R 4.2.3)
     lattice       0.21-8  2023-04-05 [1] CRAN (R 4.2.3)
     lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.2)
     magrittr    * 2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
     Matrix        1.5-4   2023-04-04 [1] CRAN (R 4.2.3)
     mr.raps       0.4.1   2023-04-18 [1] Github (qingyuanzhao/mr.raps@2a23d84)
     munsell       0.5.0   2018-06-12 [2] CRAN (R 4.0.3)
     nortest       1.0-4   2015-07-30 [2] CRAN (R 4.0.3)
     pillar        1.9.0   2023-03-22 [1] CRAN (R 4.2.3)
     pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 4.0.3)
     plyr          1.8.8   2022-11-11 [1] CRAN (R 4.2.3)
     R6            2.5.1   2021-08-19 [2] CRAN (R 4.1.1)
     Rcpp          1.0.10  2023-01-22 [1] CRAN (R 4.2.2)
     rlang         1.1.0   2023-03-14 [1] CRAN (R 4.2.2)
     rmarkdown     2.21    2023-03-26 [1] CRAN (R 4.2.3)
     rootSolve     1.8.2.3 2021-09-29 [2] CRAN (R 4.1.1)
     rprojroot     2.0.3   2022-04-02 [2] CRAN (R 4.2.0)
     rsnps         0.5.0.0 2022-01-28 [1] CRAN (R 4.2.3)
     scales        1.2.1   2022-08-20 [1] CRAN (R 4.2.3)
     sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
     shape         1.4.6   2021-05-19 [2] CRAN (R 4.1.1)
     stringi       1.7.12  2023-01-11 [1] CRAN (R 4.2.2)
     stringr       1.5.0   2022-12-02 [1] CRAN (R 4.2.3)
     survival      3.5-5   2023-03-12 [1] CRAN (R 4.2.3)
     tibble        3.2.1   2023-03-20 [1] CRAN (R 4.2.3)
     tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.2.2)
     TwoSampleMR   0.5.6   2023-04-13 [1] Github (MRCIEU/TwoSampleMR@f856a15)
     tzdb          0.3.0   2022-03-28 [2] CRAN (R 4.2.0)
     utf8          1.2.3   2023-01-31 [1] CRAN (R 4.2.3)
     vctrs         0.6.1   2023-03-22 [1] CRAN (R 4.2.3)
     vroom         1.6.1   2023-01-22 [1] CRAN (R 4.2.2)
     withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
     xfun          0.38    2023-03-24 [1] CRAN (R 4.2.3)
     yaml          2.3.7   2023-01-23 [1] CRAN (R 4.2.3)

     [1] /net/mulan/home/fredboe/R/x86_64-pc-linux-gnu-library/4.0
     [2] /net/mario/cluster/lib/R/site-library-bionic-40
     [3] /usr/local/lib/R/site-library
     [4] /usr/lib/R/site-library
     [5] /usr/lib/R/library

    ──────────────────────────────────────────────────────────────────────────────

``` r
# git commit info
gr <- git2r::repository(here::here()) %>%
    git2r::commits()
gr[[1]] 
```

    [470ad54] 2023-05-12: fix: updated Neale lab UKB MR analysis to use female only gwas summary files
