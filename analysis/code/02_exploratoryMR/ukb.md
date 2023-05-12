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
        print(knitr::kable(result))
    }
}
```

    API: public: http://gwas-api.mrcieu.ac.uk/

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping tol8yC, 7 variants, using EUR population reference

    Removing 4 of 7 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (qVGcCp)

    Analysing 'tol8yC' on 'qVGcCp'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | qVGcCp     | outcome | exposure | MR Egger                  |    3 |  0.0452620 | 0.0834409 | 0.6835853 |
| tol8yC      | qVGcCp     | outcome | exposure | Weighted median           |    3 | -0.0063779 | 0.0081030 | 0.4312241 |
| tol8yC      | qVGcCp     | outcome | exposure | Inverse variance weighted |    3 | -0.0116360 | 0.0101951 | 0.2537287 |
| tol8yC      | qVGcCp     | outcome | exposure | Simple mode               |    3 | -0.0029696 | 0.0098869 | 0.7922474 |
| tol8yC      | qVGcCp     | outcome | exposure | Weighted mode             |    3 | -0.0041515 | 0.0081245 | 0.6601819 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (pKIvRa)

    Analysing 'tol8yC' on 'pKIvRa'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | pKIvRa     | outcome | exposure | MR Egger                  |    3 |  0.1190633 | 0.0404611 | 0.2085469 |
| tol8yC      | pKIvRa     | outcome | exposure | Weighted median           |    3 |  0.0017131 | 0.0075644 | 0.8208358 |
| tol8yC      | pKIvRa     | outcome | exposure | Inverse variance weighted |    3 |  0.0016671 | 0.0119379 | 0.8889355 |
| tol8yC      | pKIvRa     | outcome | exposure | Simple mode               |    3 | -0.0094901 | 0.0136733 | 0.5594227 |
| tol8yC      | pKIvRa     | outcome | exposure | Weighted mode             |    3 |  0.0083734 | 0.0126463 | 0.5759816 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (vdXmwg)

    Analysing 'tol8yC' on 'vdXmwg'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | vdXmwg     | outcome | exposure | MR Egger                  |    3 |  0.0033322 | 0.0022448 | 0.3774020 |
| tol8yC      | vdXmwg     | outcome | exposure | Weighted median           |    3 |  0.0001662 | 0.0002589 | 0.5207661 |
| tol8yC      | vdXmwg     | outcome | exposure | Inverse variance weighted |    3 | -0.0001231 | 0.0004176 | 0.7681797 |
| tol8yC      | vdXmwg     | outcome | exposure | Simple mode               |    3 |  0.0001893 | 0.0003278 | 0.6219760 |
| tol8yC      | vdXmwg     | outcome | exposure | Weighted mode             |    3 |  0.0002079 | 0.0002481 | 0.4902259 |

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping tol8yC, 27 variants, using EUR population reference

    Removing 23 of 27 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (vxZg7e)

    Analysing 'tol8yC' on 'vxZg7e'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | vxZg7e     | outcome | exposure | MR Egger                  |    4 |  0.0496276 | 0.0532251 | 0.4495575 |
| tol8yC      | vxZg7e     | outcome | exposure | Weighted median           |    4 | -0.0041630 | 0.0069698 | 0.5503117 |
| tol8yC      | vxZg7e     | outcome | exposure | Inverse variance weighted |    4 | -0.0093667 | 0.0078296 | 0.2315698 |
| tol8yC      | vxZg7e     | outcome | exposure | Simple mode               |    4 | -0.0010033 | 0.0084898 | 0.9133926 |
| tol8yC      | vxZg7e     | outcome | exposure | Weighted mode             |    4 | -0.0027275 | 0.0080411 | 0.7568201 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (UtP15c)

    Analysing 'tol8yC' on 'UtP15c'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | UtP15c     | outcome | exposure | MR Egger                  |    4 |  0.0829785 | 0.0515057 | 0.2484739 |
| tol8yC      | UtP15c     | outcome | exposure | Weighted median           |    4 | -0.0057269 | 0.0079091 | 0.4690110 |
| tol8yC      | UtP15c     | outcome | exposure | Inverse variance weighted |    4 | -0.0008171 | 0.0091070 | 0.9285072 |
| tol8yC      | UtP15c     | outcome | exposure | Simple mode               |    4 | -0.0104251 | 0.0110174 | 0.4138360 |
| tol8yC      | UtP15c     | outcome | exposure | Weighted mode             |    4 | -0.0093331 | 0.0142287 | 0.5586855 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (sLgx7Z)

    Analysing 'tol8yC' on 'sLgx7Z'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | sLgx7Z     | outcome | exposure | MR Egger                  |    4 |  0.0031264 | 0.0014522 | 0.1642015 |
| tol8yC      | sLgx7Z     | outcome | exposure | Weighted median           |    4 |  0.0001739 | 0.0002358 | 0.4607739 |
| tol8yC      | sLgx7Z     | outcome | exposure | Inverse variance weighted |    4 | -0.0000602 | 0.0003114 | 0.8466688 |
| tol8yC      | sLgx7Z     | outcome | exposure | Simple mode               |    4 |  0.0001761 | 0.0002683 | 0.5583650 |
| tol8yC      | sLgx7Z     | outcome | exposure | Weighted mode             |    4 |  0.0002056 | 0.0002554 | 0.4797211 |

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping tol8yC, 88 variants, using EUR population reference

    Removing 78 of 88 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (mVCCdH)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'tol8yC' on 'mVCCdH'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | mVCCdH     | outcome | exposure | MR Egger                  |    9 | -0.0077117 | 0.0370119 | 0.8408821 |
| tol8yC      | mVCCdH     | outcome | exposure | Weighted median           |    9 | -0.0054017 | 0.0063538 | 0.3952400 |
| tol8yC      | mVCCdH     | outcome | exposure | Inverse variance weighted |    9 | -0.0056231 | 0.0070805 | 0.4270969 |
| tol8yC      | mVCCdH     | outcome | exposure | Simple mode               |    9 | -0.0043119 | 0.0094221 | 0.6593854 |
| tol8yC      | mVCCdH     | outcome | exposure | Weighted mode             |    9 | -0.0043119 | 0.0077559 | 0.5934476 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (hmOUkB)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'tol8yC' on 'hmOUkB'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | hmOUkB     | outcome | exposure | MR Egger                  |    9 |  0.0248932 | 0.0275548 | 0.3963324 |
| tol8yC      | hmOUkB     | outcome | exposure | Weighted median           |    9 | -0.0043895 | 0.0061043 | 0.4720895 |
| tol8yC      | hmOUkB     | outcome | exposure | Inverse variance weighted |    9 |  0.0021690 | 0.0055307 | 0.6949327 |
| tol8yC      | hmOUkB     | outcome | exposure | Simple mode               |    9 |  0.0169004 | 0.0128925 | 0.2262773 |
| tol8yC      | hmOUkB     | outcome | exposure | Weighted mode             |    9 |  0.0170935 | 0.0133942 | 0.2376884 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (tol8yC) and outcome (DWT0Fe)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'tol8yC' on 'DWT0Fe'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| tol8yC      | DWT0Fe     | outcome | exposure | MR Egger                  |    9 |  0.0015896 | 0.0007178 | 0.0623772 |
| tol8yC      | DWT0Fe     | outcome | exposure | Weighted median           |    9 |  0.0001531 | 0.0002035 | 0.4519417 |
| tol8yC      | DWT0Fe     | outcome | exposure | Inverse variance weighted |    9 | -0.0000879 | 0.0001707 | 0.6065618 |
| tol8yC      | DWT0Fe     | outcome | exposure | Simple mode               |    9 |  0.0002152 | 0.0002730 | 0.4532546 |
| tol8yC      | DWT0Fe     | outcome | exposure | Weighted mode             |    9 |  0.0002104 | 0.0002504 | 0.4252359 |

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

    [5aab38c] 2023-05-12: feat: rendered qmd to gfm
