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
        pp <- TwoSampleMR::mr_scatter_plot(mr_results = result, dat = dat_harm)
        print(pp[[1]])
    }
}
```

    API: public: http://gwas-api.mrcieu.ac.uk/

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping JbKImT, 7 variants, using EUR population reference

    Removing 4 of 7 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (Lv6AIG)

    Analysing 'JbKImT' on 'Lv6AIG'

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | Lv6AIG     | outcome | exposure | MR Egger                  |    3 |  0.0452620 | 0.0834409 | 0.6835853 |
| JbKImT      | Lv6AIG     | outcome | exposure | Weighted median           |    3 | -0.0063779 | 0.0081710 | 0.4350643 |
| JbKImT      | Lv6AIG     | outcome | exposure | Inverse variance weighted |    3 | -0.0116360 | 0.0101951 | 0.2537287 |
| JbKImT      | Lv6AIG     | outcome | exposure | Simple mode               |    3 | -0.0029696 | 0.0098092 | 0.7906744 |
| JbKImT      | Lv6AIG     | outcome | exposure | Weighted mode             |    3 | -0.0041515 | 0.0085692 | 0.6759191 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (Cwjln6)

    Analysing 'JbKImT' on 'Cwjln6'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-1.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | Cwjln6     | outcome | exposure | MR Egger                  |    3 |  0.1190633 | 0.0404611 | 0.2085469 |
| JbKImT      | Cwjln6     | outcome | exposure | Weighted median           |    3 |  0.0017131 | 0.0076857 | 0.8236164 |
| JbKImT      | Cwjln6     | outcome | exposure | Inverse variance weighted |    3 |  0.0016671 | 0.0119379 | 0.8889355 |
| JbKImT      | Cwjln6     | outcome | exposure | Simple mode               |    3 | -0.0094901 | 0.0144798 | 0.5795178 |
| JbKImT      | Cwjln6     | outcome | exposure | Weighted mode             |    3 |  0.0083734 | 0.0137425 | 0.6043186 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (bqVvBe)

    Analysing 'JbKImT' on 'bqVvBe'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-2.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-08

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | bqVvBe     | outcome | exposure | MR Egger                  |    3 |  0.0033322 | 0.0022448 | 0.3774020 |
| JbKImT      | bqVvBe     | outcome | exposure | Weighted median           |    3 |  0.0001662 | 0.0002750 | 0.5455868 |
| JbKImT      | bqVvBe     | outcome | exposure | Inverse variance weighted |    3 | -0.0001231 | 0.0004176 | 0.7681797 |
| JbKImT      | bqVvBe     | outcome | exposure | Simple mode               |    3 |  0.0001893 | 0.0003196 | 0.6137123 |
| JbKImT      | bqVvBe     | outcome | exposure | Weighted mode             |    3 |  0.0002079 | 0.0002531 | 0.4977459 |

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping JbKImT, 27 variants, using EUR population reference

    Removing 23 of 27 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (ZmxyJm)

    Analysing 'JbKImT' on 'ZmxyJm'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-3.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | ZmxyJm     | outcome | exposure | MR Egger                  |    4 |  0.0496276 | 0.0532251 | 0.4495575 |
| JbKImT      | ZmxyJm     | outcome | exposure | Weighted median           |    4 | -0.0041630 | 0.0067009 | 0.5344270 |
| JbKImT      | ZmxyJm     | outcome | exposure | Inverse variance weighted |    4 | -0.0093667 | 0.0078296 | 0.2315698 |
| JbKImT      | ZmxyJm     | outcome | exposure | Simple mode               |    4 | -0.0010033 | 0.0084914 | 0.9134091 |
| JbKImT      | ZmxyJm     | outcome | exposure | Weighted mode             |    4 | -0.0027275 | 0.0075438 | 0.7416507 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (BrlJdP)

    Analysing 'JbKImT' on 'BrlJdP'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-4.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | BrlJdP     | outcome | exposure | MR Egger                  |    4 |  0.0829785 | 0.0515057 | 0.2484739 |
| JbKImT      | BrlJdP     | outcome | exposure | Weighted median           |    4 | -0.0057269 | 0.0080272 | 0.4755739 |
| JbKImT      | BrlJdP     | outcome | exposure | Inverse variance weighted |    4 | -0.0008171 | 0.0091070 | 0.9285072 |
| JbKImT      | BrlJdP     | outcome | exposure | Simple mode               |    4 | -0.0104251 | 0.0110598 | 0.4154182 |
| JbKImT      | BrlJdP     | outcome | exposure | Weighted mode             |    4 | -0.0093331 | 0.0142681 | 0.5597031 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (qEv7sX)

    Analysing 'JbKImT' on 'qEv7sX'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-5.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-07

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | qEv7sX     | outcome | exposure | MR Egger                  |    4 |  0.0031264 | 0.0014522 | 0.1642015 |
| JbKImT      | qEv7sX     | outcome | exposure | Weighted median           |    4 |  0.0001739 | 0.0002401 | 0.4688647 |
| JbKImT      | qEv7sX     | outcome | exposure | Inverse variance weighted |    4 | -0.0000602 | 0.0003114 | 0.8466688 |
| JbKImT      | qEv7sX     | outcome | exposure | Simple mode               |    4 |  0.0001761 | 0.0002577 | 0.5433693 |
| JbKImT      | qEv7sX     | outcome | exposure | Weighted mode             |    4 |  0.0002056 | 0.0002495 | 0.4703928 |

    Please look at vignettes for options on running this locally if you need to run many instances of this command.

    Clumping JbKImT, 88 variants, using EUR population reference

    Removing 78 of 88 variants due to LD with other variants or absence from LD reference panel

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (VtacK9)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'JbKImT' on 'VtacK9'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-6.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30700_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | VtacK9     | outcome | exposure | MR Egger                  |    9 | -0.0077117 | 0.0370119 | 0.8408821 |
| JbKImT      | VtacK9     | outcome | exposure | Weighted median           |    9 | -0.0054017 | 0.0061945 | 0.3832010 |
| JbKImT      | VtacK9     | outcome | exposure | Inverse variance weighted |    9 | -0.0056231 | 0.0070805 | 0.4270969 |
| JbKImT      | VtacK9     | outcome | exposure | Simple mode               |    9 | -0.0043119 | 0.0095861 | 0.6647882 |
| JbKImT      | VtacK9     | outcome | exposure | Weighted mode             |    9 | -0.0043119 | 0.0079093 | 0.6005026 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (HVQ2vL)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'JbKImT' on 'HVQ2vL'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-7.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/30720_irnt.gwas.imputed_v3.female.varorder.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | HVQ2vL     | outcome | exposure | MR Egger                  |    9 |  0.0248932 | 0.0275548 | 0.3963324 |
| JbKImT      | HVQ2vL     | outcome | exposure | Weighted median           |    9 | -0.0043895 | 0.0065126 | 0.5003141 |
| JbKImT      | HVQ2vL     | outcome | exposure | Inverse variance weighted |    9 |  0.0021690 | 0.0055307 | 0.6949327 |
| JbKImT      | HVQ2vL     | outcome | exposure | Simple mode               |    9 |  0.0169004 | 0.0135943 | 0.2489930 |
| JbKImT      | HVQ2vL     | outcome | exposure | Weighted mode             |    9 |  0.0170935 | 0.0132351 | 0.2325819 |

    No phenotype name specified, defaulting to 'outcome'.

    Warning in TwoSampleMR::format_data(., type = "outcome", effect_allele_col = "A1", : The following columns are not present but are helpful for harmonisation
    eaf

    Harmonising exposure (JbKImT) and outcome (7rrOD7)

    Removing the following SNPs for being palindromic with intermediate allele frequencies:
    rs72802873

    Analysing 'JbKImT' on '7rrOD7'

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-8.png)

</div>

## /net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_for_munge_sumstats/N18.gwas.imputed_v3.female.tsv.gz

### pvalue threshold: 1e-06

| id.exposure | id.outcome | outcome | exposure | method                    | nsnp |          b |        se |      pval |
|:------------|:-----------|:--------|:---------|:--------------------------|-----:|-----------:|----------:|----------:|
| JbKImT      | 7rrOD7     | outcome | exposure | MR Egger                  |    9 |  0.0015896 | 0.0007178 | 0.0623772 |
| JbKImT      | 7rrOD7     | outcome | exposure | Weighted median           |    9 |  0.0001531 | 0.0002048 | 0.4547700 |
| JbKImT      | 7rrOD7     | outcome | exposure | Inverse variance weighted |    9 | -0.0000879 | 0.0001707 | 0.6065618 |
| JbKImT      | 7rrOD7     | outcome | exposure | Simple mode               |    9 |  0.0002152 | 0.0002745 | 0.4556184 |
| JbKImT      | 7rrOD7     | outcome | exposure | Weighted mode             |    9 |  0.0002104 | 0.0002306 | 0.3882588 |

<div class="cell-output-display">

![](ukb_files/figure-commonmark/unnamed-chunk-3-9.png)

</div>

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
     farver        2.1.1   2022-07-06 [1] CRAN (R 4.2.3)
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
     labeling      0.4.2   2020-10-20 [2] CRAN (R 4.0.3)
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

    [f989ceb] 2023-05-12: feat: rendered qmd after enclosing knitr kable calls in print() statements
