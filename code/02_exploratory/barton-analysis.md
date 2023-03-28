Barton et al. GWAS summary statistics & FMD GWAS meta-analysis summary
statistics
================
Fred Boehm

- <a href="#cystatinc-with-p-value-threshold-1e-06"
  id="toc-cystatinc-with-p-value-threshold-1e-06"><span
  class="toc-section-number">1</span>
  <code>CystatinC with p-value threshold:  1e-06</code></a>
- <a href="#cystatinc-with-p-value-threshold-1e-07"
  id="toc-cystatinc-with-p-value-threshold-1e-07"><span
  class="toc-section-number">2</span>
  <code>CystatinC with p-value threshold:  1e-07</code></a>
- <a href="#cystatinc-with-p-value-threshold-1e-08"
  id="toc-cystatinc-with-p-value-threshold-1e-08"><span
  class="toc-section-number">3</span>
  <code>CystatinC with p-value threshold:  1e-08</code></a>
- <a href="#creatinine-with-p-value-threshold-1e-06"
  id="toc-creatinine-with-p-value-threshold-1e-06"><span
  class="toc-section-number">4</span>
  <code>Creatinine with p-value threshold:  1e-06</code></a>
- <a href="#creatinine-with-p-value-threshold-1e-07"
  id="toc-creatinine-with-p-value-threshold-1e-07"><span
  class="toc-section-number">5</span>
  <code>Creatinine with p-value threshold:  1e-07</code></a>
- <a href="#creatinine-with-p-value-threshold-1e-08"
  id="toc-creatinine-with-p-value-threshold-1e-08"><span
  class="toc-section-number">6</span>
  <code>Creatinine with p-value threshold:  1e-08</code></a>

We want to “explore” the two results files to see how many SNPs are
available in each, and to see how many SNPs are in both files. This
information will inform 2-sample MR studies to assess the causal effect
of FMD on creatinine levels.

First, we read into R the FMD summary statistics file.

``` r
fmd_file <- here::here("data", "fmd", "GCST90026612_buildGRCh37.tsv")
fmd_tib <- vroom::vroom(fmd_file)
```

    Rows: 5483710 Columns: 12
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (3): SNP, OA, EA
    dbl (9): chromosome, base_pair_location, EAF, BETA, SE, p_value, Het_P, N_ca...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
library(magrittr)
```

``` r
# https://cran.r-project.org/web/packages/LDlinkR/vignettes/LDlinkR.html
# follow steps at above url to use LDlinkR to get LD matrix for use with MendelianRandomization R pkg
ld_token <- "3fbdb0673b22"
barton_sample_size <- 437660 # https://www.ebi.ac.uk/gwas/efotraits/EFO_0004518 - see Barton et al. 2021
# define function
remove_offdiagonal_ones <- function(symm_matrix, threshold = 0.9){
    nr <- nrow(symm_matrix)
    bad_list <- list()
    for (row in 1:nr){
        foo <- upper.tri(symm_matrix, diag = FALSE)[row,]
        bar <- symm_matrix[row, ] > threshold
        bad_list[[row]] <- which(foo & bar)
    }
    bad_indices <- unique(do.call("c", bad_list))
    if (length(bad_indices) == 0){
        return(symm_matrix)
    } else {
        out <- as.matrix(symm_matrix[- bad_indices, - bad_indices])
        rownames(out) <- colnames(out) <- colnames(symm_matrix)[- bad_indices]
        return(out)
    }
}
```

``` r
outcomes <- c("CystatinC", "Creatinine")
p_thresholds <- c(1e-06, 1e-07, 1e-08)
ld_file <- here::here("data", "fmd", "ld_matrix.rds")
for (outcome in outcomes){
    for (p_threshold in p_thresholds){
        if (outcome == "CystatinC"){
            barton_file <- here::here("data", "barton2021_cystatinC", "GCST90025945_buildGRCh37.tsv")
        }
        if (outcome == "Creatinine"){
            barton_file <- here::here("data", "barton2021_creatinine", "GCST90025946_buildGRCh37.tsv")
        }
        barton_tib <- vroom::vroom(barton_file)
        # find shared snps
        small_dat_pre <- fmd_tib %>%
            dplyr::filter(p_value < p_threshold)  %>% # choose pvalue threshold     
            dplyr::inner_join(barton_tib, by = c("chromosome", "base_pair_location"))
        # harmonised? 
        small_dat <- small_dat_pre %>%
            dplyr::mutate(harmonised = OA == ALLELE0)
        (all.equal(small_dat$harmonised, rep(TRUE, length(small_dat$harmonised))))
        # make ld matrix if ld_file doesn't exist
        if (file.exists(ld_file)){
            ld_mat <- readRDS(ld_file)
        } else {
            ld_list <- list()
            pt <- min(p_thresholds)
            for (chr in 1:22){
                fmd_onechr <- small_dat %>%
                    dplyr::filter(chromosome == chr)
                if (nrow(fmd_onechr) > 1){
                    foo <- LDlinkR::LDmatrix(fmd_onechr$SNP, 
                            pop = "CEU", 
                            r2d = "r2", 
                            genome_build = "grch37",
                            token = ld_token, 
                            file = FALSE
                            ) 
                    
                    bar <- foo %>% 
                                dplyr::select(-1) %>%
                                as.matrix() %>%
                                remove_offdiagonal_ones(threshold = 0.99)

                    # remove rows & cols for highly correlated SNPs 

                    ld_list[[chr]] <- bar
                }
                if (nrow(fmd_onechr) == 1){
                    ld_list[[chr]] <- as.matrix(1)
                    colnames(ld_list[[chr]]) <- fmd_onechr$SNP      
                }
                if (nrow(fmd_onechr) == 0){
                    ld_list[[chr]] <- NA
                }
            }
            # remove NAs
            ld_list_nona <- ld_list[!is.na(ld_list)]
            ld_mat <- as.matrix(Matrix::bdiag(ld_list_nona))
            rn <- do.call(c, lapply(ld_list_nona, colnames))
            rownames(ld_mat) <- rn
            colnames(ld_mat) <- rn
            # save ld matrix
            saveRDS(ld_mat, ld_file)
        }

        # filter to remove highly correlated SNPs
        small_dat_no_ld <- small_dat %>%
            dplyr::filter(SNP %in% rownames(ld_mat))
        ld_mat_small <- ld_mat[rownames(ld_mat) %in% small_dat_no_ld$SNP, 
                               colnames(ld_mat) %in% small_dat_no_ld$SNP]
        print(nrow(small_dat_no_ld))
        # make input object
        input <- MendelianRandomization::mr_input(
                    bx = small_dat_no_ld$BETA, 
                    bxse = small_dat_no_ld$SE, 
                    by = small_dat_no_ld$beta, 
                    byse = small_dat_no_ld$standard_error,
                    corr = ld_mat_small,
                    exposure = "FMD",
                    outcome = outcome,
                    snps = rownames(ld_mat_small)
                )
        # make header for collection of graphs for each iteration of loop
        cat('\n\n## `', outcome, "with p-value threshold: ", p_threshold, '`\n\n')    
        # MR analyses   
        result <- MendelianRandomization::mr_allmethods(input)
        result@Values %>% 
            dplyr::mutate_if(is.numeric, signif, digits = 2) %>%
            dplyr::mutate("Number of SNPs" = length(result@Data@snps)) %>%
            dplyr::rename("95% CI lower" = 4, "95% CI upper" = 5) %>%
            knitr::kable() %>%
            print()
        ivw_res <- MendelianRandomization::mr_ivw(input, correl = TRUE)
        egger_res <- MendelianRandomization::mr_egger(input, correl = TRUE)

        # MR plots
        MendelianRandomization::mr_plot(input, interactive = FALSE, line = "ivw", labels = TRUE, orientate = TRUE) %>%
            print()
        MendelianRandomization::mr_plot(input, interactive = FALSE, line = "egger", labels = TRUE, orientate = TRUE) %>%
            print()
        ## MR.SPI
       res_mrspi <- MR.SPI::MR.SPI(gamma = small_dat_no_ld$BETA, #FMD beta. lowercase gamma is the exposure
                        se_gamma = small_dat_no_ld$SE, #FMD se
                        Gamma = small_dat_no_ld$beta, #outcome beta
                        se_Gamma = small_dat_no_ld$standard_error, #outcome se
                        n1 = small_dat_no_ld$N_cases[1] + small_dat_no_ld$N_ctrls[1],# FMD sample size
                        n2 = barton_sample_size, #outcome sample size
                        unif = TRUE
        ) 
        res_mrspi %>%
            print()
        # print only the valid SNPs
        small_dat_no_ld[res_mrspi$VHat, ] %>%
            dplyr::select(chromosome, base_pair_location, SNP, BETA, SE, p_value.x, p_value.y) %>%
            knitr::kable() %>%
            print()

        # do clustering (& MRAHC) when I have at least ten snps in the dataset
        if (nrow(small_dat_no_ld) >= 10){
            ## devtools::install_github("xiaoran-liang/MRAHC")
            res_mrahc_a <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 4,
                                        outremove = FALSE)
            res_mrahc_b <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 3,
                                        outremove = FALSE)
            res_mrahc_c <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 4,
                                        outremove = TRUE)
            res_mrahc_d <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 3,
                                        outremove = TRUE)
            res_mrahc_e <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 4,
                                        outremove = TRUE, 
                                        iter = TRUE)
            res_mrahc_f <- MRAHC::MR_AHC(betaX = small_dat_no_ld$BETA, 
                                        seX = small_dat_no_ld$SE,
                                        betaY = small_dat_no_ld$beta,
                                        seY = small_dat_no_ld$standard_error,
                                        n = barton_sample_size,
                                        smallcluster = 3,
                                        outremove = TRUE,
                                        iter = TRUE)
        
                                    # NOTE FROM MRAHC PACKAGE DOCUMENTATION: #'In a two-sample MR design, we recommend using the sample size of the outcome sample.
            res_mrahc_a %>%
                print()
            res_mrahc_b %>%
                print()
            res_mrahc_c %>%
                print()
            res_mrahc_d %>%
                print()
            res_mrahc_e %>%
                print()
            res_mrahc_f %>%
                print()
        }
    }
}
```

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

\[1\] 10

## `CystatinC with p-value threshold:  1e-06`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |  -0.0120 |    0.0043 |     -2.0e-02 |      -0.0034 | 5.8e-03 |             10 |
| Weighted median           |   0.0051 |    0.0042 |     -3.1e-03 |       0.0130 | 2.2e-01 |             10 |
| Penalized weighted median |   0.0073 |    0.0037 |      5.9e-06 |       0.0150 | 5.0e-02 |             10 |
| IVW                       |  -0.0270 |    0.0039 |     -3.5e-02 |      -0.0190 | 0.0e+00 |             10 |
| Penalized IVW             |  -0.0270 |    0.0039 |     -3.5e-02 |      -0.0190 | 0.0e+00 |             10 |
| Robust IVW                |  -0.0270 |    0.0039 |     -3.5e-02 |      -0.0190 | 0.0e+00 |             10 |
| Penalized robust IVW      |  -0.0270 |    0.0039 |     -3.5e-02 |      -0.0190 | 0.0e+00 |             10 |
| MR-Egger                  |   0.0670 |    0.0130 |      4.1e-02 |       0.0930 | 6.0e-07 |             10 |
| (intercept)               |  -0.0230 |    0.0032 |     -2.9e-02 |      -0.0160 | 0.0e+00 |             10 |
| Penalized MR-Egger        |   0.0670 |    0.0130 |      4.1e-02 |       0.0930 | 6.0e-07 |             10 |
| (intercept)               |  -0.0230 |    0.0032 |     -2.9e-02 |      -0.0160 | 0.0e+00 |             10 |
| Robust MR-Egger           |   0.0670 |    0.0130 |      4.1e-02 |       0.0930 | 6.0e-07 |             10 |
| (intercept)               |  -0.0230 |    0.0032 |     -2.9e-02 |      -0.0160 | 0.0e+00 |             10 |
| Penalized robust MR-Egger |   0.0670 |    0.0130 |      4.1e-02 |       0.0930 | 6.0e-07 |             10 |
| (intercept)               |  -0.0230 |    0.0032 |     -2.9e-02 |      -0.0160 | 0.0e+00 |             10 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-1.png)

</div>

10 SNPs used as candidate instruments. MR.SPI identifies 10 relevant
instruments and 6 valid instruments. Estimated Causal Effect by MR.SPI:
-0.014 Uniform Confidence Interval: (-0.048 , -0.013)

\$betaHat \[1\] -0.01355321

\$beta.sdHat \[,1\] \[1,\] 0.005751408

\$ci \[,1\] \[,2\] \[1,\] -0.04792737 -0.01319778

\$SHat \[1\] 1 2 3 4 5 6 7 8 9 10

\$VHat \[1\] 1 3 4 5 6 7

\$voting.mat 1 2 3 4 5 6 7 8 9 10 1 1 1 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0
1 1 1 3 1 0 1 1 1 1 1 0 0 0 4 1 0 1 1 1 1 1 0 0 0 5 1 0 1 1 1 1 1 0 0 0
6 1 0 1 1 1 1 1 0 0 0 7 1 0 1 1 1 1 1 0 0 0 8 1 1 0 0 0 0 0 1 1 1 9 1 1
0 0 0 0 0 1 1 1 10 1 1 0 0 0 0 0 1 1 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          5 |          152248145 | rs72802886 |  0.5223 | 0.1038 |     5e-07 |   5.9e-01 |
|         12 |           50503269 | rs836180   |  0.2362 | 0.0442 |     1e-07 |   8.5e-05 |
|         12 |           50529936 | rs7136570  | -0.2261 | 0.0440 |     3e-07 |   4.9e-05 |
|         12 |           50537815 | rs7302981  | -0.2266 | 0.0438 |     2e-07 |   1.0e-04 |
|         12 |           50594947 | rs4459386  | -0.2545 | 0.0436 |     0e+00 |   8.5e-05 |
|         12 |           51094138 | rs2731443  |  0.2273 | 0.0445 |     3e-07 |   1.7e-04 |

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.007937609
0.003134577 2.532274 0.9577157 0 \[2,\] 2 5 -0.030092182 0.004585964
6.561800 0.9952389 0

\$confidence_interval lower upper Cluster1 0.00179395 0.01408127
Cluster2 -0.03908051 -0.02110386

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-2.png)

</div>

\[1\] 6

## `CystatinC with p-value threshold:  1e-07`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |   0.0069 |    0.0039 |     -0.00066 |       0.0140 | 7.3e-02 |              6 |
| Weighted median           |   0.0069 |    0.0038 |     -0.00058 |       0.0140 | 7.1e-02 |              6 |
| Penalized weighted median |   0.0077 |    0.0038 |      0.00029 |       0.0150 | 4.2e-02 |              6 |
| IVW                       |  -0.0120 |    0.0081 |     -0.02800 |       0.0039 | 1.4e-01 |              6 |
| Penalized IVW             |  -0.0120 |    0.0081 |     -0.02800 |       0.0039 | 1.4e-01 |              6 |
| Robust IVW                |  -0.0120 |    0.0081 |     -0.02800 |       0.0039 | 1.4e-01 |              6 |
| Penalized robust IVW      |  -0.0120 |    0.0081 |     -0.02800 |       0.0039 | 1.4e-01 |              6 |
| MR-Egger                  |   0.0900 |    0.0190 |      0.05200 |       0.1300 | 3.5e-06 |              6 |
| (intercept)               |  -0.0290 |    0.0054 |     -0.03900 |      -0.0180 | 1.0e-07 |              6 |
| Penalized MR-Egger        |   0.0900 |    0.0190 |      0.05200 |       0.1300 | 3.5e-06 |              6 |
| (intercept)               |  -0.0290 |    0.0054 |     -0.03900 |      -0.0180 | 1.0e-07 |              6 |
| Robust MR-Egger           |   0.0900 |    0.0190 |      0.05200 |       0.1300 | 3.5e-06 |              6 |
| (intercept)               |  -0.0290 |    0.0054 |     -0.03900 |      -0.0180 | 1.0e-07 |              6 |
| Penalized robust MR-Egger |   0.0900 |    0.0190 |      0.05200 |       0.1300 | 3.5e-06 |              6 |
| (intercept)               |  -0.0290 |    0.0054 |     -0.03900 |      -0.0180 | 1.0e-07 |              6 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-3.png)

</div>

6 SNPs used as candidate instruments. MR.SPI identifies 6 relevant
instruments and 4 valid instruments. Estimated Causal Effect by MR.SPI:
0.009 Uniform Confidence Interval: (0 , 0.022)

\$betaHat \[1\] 0.008983117

\$beta.sdHat \[,1\] \[1,\] 0.003341228

\$ci \[,1\] \[,2\] \[1,\] -0.0001466134 0.02155938

\$SHat \[1\] 1 2 3 4 5 6

\$VHat \[1\] 1 4 5 6

\$voting.mat 1 2 3 4 5 6 1 1 0 0 1 1 1 2 0 1 1 0 0 0 3 0 1 1 0 0 0 4 1 0
0 1 1 1 5 1 0 0 1 1 1 6 1 0 0 1 1 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          6 |           12903957 | rs9349379  | -0.3632 | 0.0464 |         0 |     0.140 |
|         12 |           57527283 | rs11172113 | -0.2913 | 0.0458 |         0 |     0.420 |
|         12 |           90008959 | rs2681472  | -0.3522 | 0.0634 |         0 |     0.120 |
|         12 |           90013089 | rs2681492  | -0.3578 | 0.0634 |         0 |     0.092 |

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-4.png)

</div>

\[1\] 3

## `CystatinC with p-value threshold:  1e-08`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |  0.00640 |    0.0051 |      -0.0036 |       0.0160 |    0.21 |              3 |
| Weighted median           |  0.00650 |    0.0047 |      -0.0026 |       0.0160 |    0.16 |              3 |
| Penalized weighted median |  0.00680 |    0.0045 |      -0.0020 |       0.0160 |    0.13 |              3 |
| IVW                       | -0.00085 |    0.0037 |      -0.0081 |       0.0064 |    0.82 |              3 |
| Penalized IVW             | -0.00085 |    0.0037 |      -0.0081 |       0.0064 |    0.82 |              3 |
| Robust IVW                | -0.00085 |    0.0037 |      -0.0081 |       0.0064 |    0.82 |              3 |
| Penalized robust IVW      | -0.00085 |    0.0037 |      -0.0081 |       0.0064 |    0.82 |              3 |
| MR-Egger                  |  0.07300 |    0.0610 |      -0.0460 |       0.1900 |    0.23 |              3 |
| (intercept)               | -0.02300 |    0.0190 |      -0.0600 |       0.0140 |    0.22 |              3 |
| Penalized MR-Egger        |  0.07300 |    0.0610 |      -0.0460 |       0.1900 |    0.23 |              3 |
| (intercept)               | -0.02300 |    0.0190 |      -0.0600 |       0.0140 |    0.22 |              3 |
| Robust MR-Egger           |  0.07300 |    0.0610 |      -0.0460 |       0.1900 |    0.23 |              3 |
| (intercept)               | -0.02300 |    0.0190 |      -0.0600 |       0.0140 |    0.22 |              3 |
| Penalized robust MR-Egger |  0.07300 |    0.0610 |      -0.0460 |       0.1900 |    0.23 |              3 |
| (intercept)               | -0.02300 |    0.0190 |      -0.0600 |       0.0140 |    0.22 |              3 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-5.png)

</div>

3 SNPs used as candidate instruments. MR.SPI identifies 3 relevant
instruments and 2 valid instruments. Estimated Causal Effect by MR.SPI:
0.007 Uniform Confidence Interval: (-0.004 , 0.017)

\$betaHat \[1\] 0.006789435

\$beta.sdHat \[,1\] \[1,\] 0.004181811

\$ci \[,1\] \[,2\] \[1,\] -0.004487812 0.01721818

\$SHat \[1\] 1 2 3

\$VHat \[1\] 1 3

\$voting.mat 1 2 3 1 1 0 1 2 0 1 0 3 1 0 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          6 |           12903957 | rs9349379  | -0.3632 | 0.0464 |         0 |      0.14 |
|         12 |           57527283 | rs11172113 | -0.2913 | 0.0458 |         0 |      0.42 |

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-6.png)

</div>

\[1\] 10

## `Creatinine with p-value threshold:  1e-06`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |  -0.0180 |    0.0048 |       -0.027 |      -0.0083 | 2.2e-04 |             10 |
| Weighted median           |  -0.0038 |    0.0049 |       -0.013 |       0.0059 | 4.4e-01 |             10 |
| Penalized weighted median |  -0.0024 |    0.0047 |       -0.012 |       0.0068 | 6.1e-01 |             10 |
| IVW                       |  -0.0390 |    0.0052 |       -0.049 |      -0.0290 | 0.0e+00 |             10 |
| Penalized IVW             |  -0.0390 |    0.0052 |       -0.049 |      -0.0290 | 0.0e+00 |             10 |
| Robust IVW                |  -0.0390 |    0.0052 |       -0.049 |      -0.0290 | 0.0e+00 |             10 |
| Penalized robust IVW      |  -0.0390 |    0.0052 |       -0.049 |      -0.0290 | 0.0e+00 |             10 |
| MR-Egger                  |   0.0830 |    0.0200 |        0.044 |       0.1200 | 2.4e-05 |             10 |
| (intercept)               |  -0.0290 |    0.0047 |       -0.039 |      -0.0200 | 0.0e+00 |             10 |
| Penalized MR-Egger        |   0.0830 |    0.0200 |        0.044 |       0.1200 | 2.4e-05 |             10 |
| (intercept)               |  -0.0290 |    0.0047 |       -0.039 |      -0.0200 | 0.0e+00 |             10 |
| Robust MR-Egger           |   0.0830 |    0.0200 |        0.044 |       0.1200 | 2.4e-05 |             10 |
| (intercept)               |  -0.0290 |    0.0047 |       -0.039 |      -0.0200 | 0.0e+00 |             10 |
| Penalized robust MR-Egger |   0.0830 |    0.0200 |        0.044 |       0.1200 | 2.4e-05 |             10 |
| (intercept)               |  -0.0290 |    0.0047 |       -0.039 |      -0.0200 | 0.0e+00 |             10 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-7.png)

</div>

    Warning in Searching.CI.sampling(gamma, Gamma, V_gamma, V_Gamma, InitiSet =
    VHat, : Sampling Criterion not met, trasfer to Searching Method.

10 SNPs used as candidate instruments. MR.SPI identifies 10 relevant
instruments and 10 valid instruments. Estimated Causal Effect by MR.SPI:
-0.041 Uniform Confidence Interval: (-0.035 , -0.018)

\$betaHat \[1\] -0.04078254

\$beta.sdHat \[,1\] \[1,\] 0.004936994

\$ci \[,1\] \[,2\] \[1,\] -0.03509017 -0.01772538

\$SHat \[1\] 1 2 3 4 5 6 7 8 9 10

\$VHat \[1\] 1 2 3 4 5 6 7 8 9 10

\$voting.mat 1 2 3 4 5 6 7 8 9 10 1 1 1 0 0 0 1 1 1 1 1 2 1 1 0 0 0 1 0
1 1 1 3 0 0 1 1 1 1 1 0 0 0 4 0 0 1 1 1 1 1 0 0 0 5 0 0 1 1 1 1 1 0 0 0
6 1 1 1 1 1 1 1 0 0 0 7 1 0 1 1 1 1 1 0 0 0 8 1 1 0 0 0 0 0 1 1 1 9 1 1
0 0 0 0 0 1 1 1 10 1 1 0 0 0 0 0 1 1 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          5 |          152248145 | rs72802886 |  0.5223 | 0.1038 |     5e-07 |   8.6e-01 |
|          6 |           12903957 | rs9349379  | -0.3632 | 0.0464 |     0e+00 |   5.4e-01 |
|         12 |           50503269 | rs836180   |  0.2362 | 0.0442 |     1e-07 |   1.0e-07 |
|         12 |           50529936 | rs7136570  | -0.2261 | 0.0440 |     3e-07 |   0.0e+00 |
|         12 |           50537815 | rs7302981  | -0.2266 | 0.0438 |     2e-07 |   1.0e-07 |
|         12 |           50594947 | rs4459386  | -0.2545 | 0.0436 |     0e+00 |   1.4e-05 |
|         12 |           51094138 | rs2731443  |  0.2273 | 0.0445 |     3e-07 |   9.2e-06 |
|         12 |           57527283 | rs11172113 | -0.2913 | 0.0458 |     0e+00 |   3.6e-01 |
|         12 |           90008959 | rs2681472  | -0.3522 | 0.0634 |     0e+00 |   1.2e-02 |
|         12 |           90013089 | rs2681492  | -0.3578 | 0.0634 |     0e+00 |   8.8e-03 |

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

\$Cluster_number \[1\] 2

\$Cluster_number_real \[1\] 2

\$AHC_cluster \$AHC_cluster\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster\[\[2\]\] \[1\] 3 4 5 6 7

\$AHC_cluster_real \$AHC_cluster_real\[\[1\]\] \[1\] 1 2 8 9 10

\$AHC_cluster_real\[\[2\]\] \[1\] 3 4 5 6 7

\$Null_cluster \[1\] 1

\$Junk_cluster NULL

\$F \[1\] 33.16434

\$AHC_results ID length beta se t Qp I^2 \[1,\] 1 5 0.006920085
0.003214780 2.152585 0.04981035 0.5788113 \[2,\] 2 5 -0.041072037
0.005241087 7.836549 0.86641830 0.0000000

\$confidence_interval lower upper Cluster1 0.000619233 0.01322094
Cluster2 -0.051344380 -0.03079969

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-8.png)

</div>

\[1\] 6

## `Creatinine with p-value threshold:  1e-07`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |  0.00084 |    0.0043 |      -0.0075 |      0.00920 | 0.84000 |              6 |
| Weighted median           |  0.00120 |    0.0043 |      -0.0073 |      0.00960 | 0.78000 |              6 |
| Penalized weighted median |  0.00210 |    0.0050 |      -0.0076 |      0.01200 | 0.68000 |              6 |
| IVW                       | -0.01800 |    0.0098 |      -0.0370 |      0.00095 | 0.06300 |              6 |
| Penalized IVW             | -0.01800 |    0.0098 |      -0.0370 |      0.00095 | 0.06300 |              6 |
| Robust IVW                | -0.01800 |    0.0098 |      -0.0370 |      0.00095 | 0.06300 |              6 |
| Penalized robust IVW      | -0.01800 |    0.0098 |      -0.0370 |      0.00095 | 0.06300 |              6 |
| MR-Egger                  |  0.09800 |    0.0310 |       0.0380 |      0.16000 | 0.00140 |              6 |
| (intercept)               | -0.03300 |    0.0085 |      -0.0490 |     -0.01600 | 0.00013 |              6 |
| Penalized MR-Egger        |  0.09800 |    0.0310 |       0.0380 |      0.16000 | 0.00140 |              6 |
| (intercept)               | -0.03300 |    0.0085 |      -0.0490 |     -0.01600 | 0.00013 |              6 |
| Robust MR-Egger           |  0.09800 |    0.0310 |       0.0380 |      0.16000 | 0.00140 |              6 |
| (intercept)               | -0.03300 |    0.0085 |      -0.0490 |     -0.01600 | 0.00013 |              6 |
| Penalized robust MR-Egger |  0.09800 |    0.0310 |       0.0380 |      0.16000 | 0.00140 |              6 |
| (intercept)               | -0.03300 |    0.0085 |      -0.0490 |     -0.01600 | 0.00013 |              6 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-9.png)

</div>

6 SNPs used as candidate instruments. MR.SPI identifies 6 relevant
instruments and 4 valid instruments. Estimated Causal Effect by MR.SPI:
0.01 Uniform Confidence Interval: (0.001 , 0.027)

\$betaHat \[1\] 0.0100577

\$beta.sdHat \[,1\] \[1,\] 0.00349955

\$ci \[,1\] \[,2\] \[1,\] 0.0009929442 0.02704014

\$SHat \[1\] 1 2 3 4 5 6

\$VHat \[1\] 1 4 5 6

\$voting.mat 1 2 3 4 5 6 1 1 0 1 1 1 1 2 0 1 1 0 0 0 3 1 1 1 0 0 0 4 1 0
0 1 1 1 5 1 0 0 1 1 1 6 1 0 0 1 1 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          6 |           12903957 | rs9349379  | -0.3632 | 0.0464 |         0 |    0.5400 |
|         12 |           57527283 | rs11172113 | -0.2913 | 0.0458 |         0 |    0.3600 |
|         12 |           90008959 | rs2681472  | -0.3522 | 0.0634 |         0 |    0.0120 |
|         12 |           90013089 | rs2681492  | -0.3578 | 0.0634 |         0 |    0.0088 |

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-10.png)

</div>

\[1\] 3

## `Creatinine with p-value threshold:  1e-08`

| Method                    | Estimate | Std Error | 95% CI lower | 95% CI upper | P-value | Number of SNPs |
|:--------------------------|---------:|----------:|-------------:|-------------:|--------:|---------------:|
| Simple median             |  -0.0039 |    0.0054 |      -0.0140 |      0.00670 |   0.470 |              3 |
| Weighted median           |  -0.0030 |    0.0051 |      -0.0130 |      0.00690 |   0.560 |              3 |
| Penalized weighted median |  -0.0002 |    0.0046 |      -0.0092 |      0.00880 |   0.970 |              3 |
| IVW                       |  -0.0073 |    0.0038 |      -0.0150 |      0.00021 |   0.057 |              3 |
| Penalized IVW             |  -0.0073 |    0.0038 |      -0.0150 |      0.00021 |   0.057 |              3 |
| Robust IVW                |  -0.0073 |    0.0038 |      -0.0150 |      0.00021 |   0.057 |              3 |
| Penalized robust IVW      |  -0.0073 |    0.0038 |      -0.0150 |      0.00021 |   0.057 |              3 |
| MR-Egger                  |   0.0470 |    0.0760 |      -0.1000 |      0.20000 |   0.540 |              3 |
| (intercept)               |  -0.0170 |    0.0230 |      -0.0630 |      0.02900 |   0.480 |              3 |
| Penalized MR-Egger        |   0.0470 |    0.0760 |      -0.1000 |      0.20000 |   0.540 |              3 |
| (intercept)               |  -0.0170 |    0.0230 |      -0.0630 |      0.02900 |   0.480 |              3 |
| Robust MR-Egger           |   0.0470 |    0.0760 |      -0.1000 |      0.20000 |   0.540 |              3 |
| (intercept)               |  -0.0170 |    0.0230 |      -0.0630 |      0.02900 |   0.480 |              3 |
| Penalized robust MR-Egger |   0.0470 |    0.0760 |      -0.1000 |      0.20000 |   0.540 |              3 |
| (intercept)               |  -0.0170 |    0.0230 |      -0.0630 |      0.02900 |   0.480 |              3 |

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-11.png)

</div>

<div class="cell-output-display">

![](barton-analysis_files/figure-commonmark/unnamed-chunk-4-12.png)

</div>

3 SNPs used as candidate instruments. MR.SPI identifies 3 relevant
instruments and 3 valid instruments. Estimated Causal Effect by MR.SPI:
-0.013 Uniform Confidence Interval: (-0.021 , 0.009)

\$betaHat \[1\] -0.01295566

\$beta.sdHat \[,1\] \[1,\] 0.00483546

\$ci \[,1\] \[,2\] \[1,\] -0.02135082 0.009037565

\$SHat \[1\] 1 2 3

\$VHat \[1\] 1 2 3

\$voting.mat 1 2 3 1 1 1 1 2 1 1 0 3 1 0 1

| chromosome | base_pair_location | SNP        |    BETA |     SE | p_value.x | p_value.y |
|-----------:|-------------------:|:-----------|--------:|-------:|----------:|----------:|
|          6 |           12903957 | rs9349379  | -0.3632 | 0.0464 |         0 |   5.4e-01 |
|         12 |           50594947 | rs4459386  | -0.2545 | 0.0436 |         0 |   1.4e-05 |
|         12 |           57527283 | rs11172113 | -0.2913 | 0.0458 |         0 |   3.6e-01 |

``` r
devtools::session_info()
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
     date     2023-03-28
     pandoc   1.19.2.4 @ /usr/bin/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package                * version  date (UTC) lib source
     arrangements             1.1.9    2020-09-13 [2] CRAN (R 4.0.3)
     bit                      4.0.5    2022-11-15 [1] CRAN (R 4.2.2)
     bit64                    4.0.5    2020-08-30 [2] CRAN (R 4.0.3)
     cachem                   1.0.7    2023-02-24 [1] CRAN (R 4.2.3)
     callr                    3.7.3    2022-11-02 [1] CRAN (R 4.2.2)
     cli                      3.6.0    2023-01-09 [1] CRAN (R 4.2.2)
     codetools                0.2-19   2023-02-01 [1] CRAN (R 4.2.2)
     colorspace               2.1-0    2023-01-23 [1] CRAN (R 4.2.2)
     crayon                   1.5.2    2022-09-29 [1] CRAN (R 4.2.1)
     data.table               1.14.8   2023-02-17 [1] CRAN (R 4.2.2)
     DBI                      1.1.3    2022-06-18 [1] CRAN (R 4.2.2)
     DEoptimR                 1.0-11   2022-04-03 [2] CRAN (R 4.2.0)
     devtools                 2.4.5    2022-10-11 [1] CRAN (R 4.2.2)
     digest                   0.6.31   2022-12-11 [1] CRAN (R 4.2.2)
     dplyr                    1.1.0    2023-01-29 [1] CRAN (R 4.2.2)
     ellipsis                 0.3.2    2021-04-29 [2] CRAN (R 4.2.1)
     evaluate                 0.20     2023-01-17 [1] CRAN (R 4.2.2)
     fansi                    1.0.4    2023-01-22 [1] CRAN (R 4.2.2)
     farver                   2.1.1    2022-07-06 [1] CRAN (R 4.2.3)
     fastmap                  1.1.1    2023-02-24 [1] CRAN (R 4.2.3)
     foreach                  1.5.2    2022-02-02 [2] CRAN (R 4.2.0)
     fs                       1.6.1    2023-02-06 [1] CRAN (R 4.2.2)
     generics                 0.1.3    2022-07-05 [1] CRAN (R 4.2.3)
     ggplot2                  3.4.1    2023-02-10 [1] CRAN (R 4.2.2)
     glmnet                   4.1-6    2022-11-27 [1] CRAN (R 4.2.2)
     glue                     1.6.2    2022-02-24 [1] CRAN (R 4.2.0)
     gmp                      0.7-1    2023-02-07 [1] CRAN (R 4.2.2)
     gtable                   0.3.2    2023-03-17 [1] CRAN (R 4.2.3)
     here                     1.0.1    2020-12-13 [2] CRAN (R 4.1.1)
     htmltools                0.5.4    2022-12-07 [1] CRAN (R 4.2.2)
     htmlwidgets              1.6.2    2023-03-17 [1] CRAN (R 4.2.2)
     httpuv                   1.6.9    2023-02-14 [1] CRAN (R 4.2.3)
     httr                     1.4.5    2023-02-24 [1] CRAN (R 4.2.3)
     igraph                   1.4.1    2023-02-24 [1] CRAN (R 4.2.2)
     intervals                0.15.3   2023-03-20 [1] CRAN (R 4.2.3)
     iterators                1.0.14   2022-02-05 [2] CRAN (R 4.2.0)
     iterpc                   0.4.2    2020-01-10 [2] CRAN (R 4.0.3)
     jsonlite                 1.8.4    2022-12-06 [1] CRAN (R 4.2.3)
     knitr                    1.42     2023-01-25 [1] CRAN (R 4.2.3)
     labeling                 0.4.2    2020-10-20 [2] CRAN (R 4.0.3)
     later                    1.3.0    2021-08-18 [2] CRAN (R 4.1.1)
     lattice                  0.20-45  2021-09-22 [2] CRAN (R 4.1.1)
     lazyeval                 0.2.2    2019-03-15 [2] CRAN (R 4.0.3)
     lifecycle                1.0.3    2022-10-07 [1] CRAN (R 4.2.2)
     magrittr               * 2.0.3    2022-03-30 [1] CRAN (R 4.2.0)
     MASS                     7.3-58.3 2023-03-07 [1] CRAN (R 4.2.3)
     Matrix                   1.5-3    2022-11-11 [1] CRAN (R 4.2.3)
     MatrixModels             0.5-1    2022-09-11 [1] CRAN (R 4.2.3)
     memoise                  2.0.1    2021-11-26 [1] CRAN (R 4.1.2)
     MendelianRandomization   0.7.0    2023-01-09 [1] CRAN (R 4.2.2)
     mime                     0.12     2021-09-28 [2] CRAN (R 4.1.1)
     miniUI                   0.1.1.1  2018-05-18 [2] CRAN (R 4.0.3)
     MR.SPI                   0.1.0    2023-03-28 [1] Github (MinhaoYaooo/MR-SPI@7657257)
     MRAHC                    0.1.0    2023-03-28 [1] Github (xiaoran-liang/MRAHC@9c9202f)
     munsell                  0.5.0    2018-06-12 [2] CRAN (R 4.0.3)
     pillar                   1.8.1    2022-08-19 [1] CRAN (R 4.2.2)
     pkgbuild                 1.4.0    2022-11-27 [1] CRAN (R 4.2.2)
     pkgconfig                2.0.3    2019-09-22 [2] CRAN (R 4.0.3)
     pkgload                  1.3.2    2022-11-16 [1] CRAN (R 4.2.2)
     plotly                   4.10.1   2022-11-07 [1] CRAN (R 4.2.2)
     prettyunits              1.1.1    2020-01-24 [2] CRAN (R 4.0.3)
     processx                 3.8.0    2022-10-26 [1] CRAN (R 4.2.2)
     profvis                  0.3.7    2020-11-02 [1] CRAN (R 4.2.2)
     promises                 1.2.0.1  2021-02-11 [2] CRAN (R 4.0.5)
     ps                       1.7.2    2022-10-26 [1] CRAN (R 4.2.3)
     purrr                    1.0.1    2023-01-10 [1] CRAN (R 4.2.2)
     quantreg                 5.93     2022-05-02 [2] CRAN (R 4.2.0)
     R6                       2.5.1    2021-08-19 [2] CRAN (R 4.1.1)
     Rcpp                     1.0.10   2023-01-22 [1] CRAN (R 4.2.2)
     remotes                  2.4.2    2021-11-30 [1] CRAN (R 4.1.2)
     rjson                    0.2.21   2022-01-09 [2] CRAN (R 4.2.0)
     rlang                    1.1.0    2023-03-14 [1] CRAN (R 4.2.2)
     rmarkdown                2.20     2023-01-19 [1] CRAN (R 4.2.2)
     robustbase               0.95-0   2022-04-02 [2] CRAN (R 4.2.0)
     rprojroot                2.0.3    2022-04-02 [2] CRAN (R 4.2.0)
     scales                   1.2.1    2022-08-20 [1] CRAN (R 4.2.3)
     sessioninfo              1.2.2    2021-12-06 [1] CRAN (R 4.1.2)
     shape                    1.4.6    2021-05-19 [2] CRAN (R 4.1.1)
     shiny                    1.7.1    2021-10-02 [2] CRAN (R 4.1.1)
     SparseM                  1.81     2021-02-18 [2] CRAN (R 4.0.5)
     stringi                  1.7.12   2023-01-11 [1] CRAN (R 4.2.2)
     stringr                  1.5.0    2022-12-02 [1] CRAN (R 4.2.3)
     survival                 3.5-5    2023-03-12 [1] CRAN (R 4.2.3)
     tibble                   3.2.0    2023-03-08 [1] CRAN (R 4.2.2)
     tidyr                    1.3.0    2023-01-24 [1] CRAN (R 4.2.3)
     tidyselect               1.2.0    2022-10-10 [1] CRAN (R 4.2.2)
     tzdb                     0.3.0    2022-03-28 [2] CRAN (R 4.2.0)
     urlchecker               1.0.1    2021-11-30 [1] CRAN (R 4.2.2)
     usethis                  2.1.6    2022-05-25 [1] CRAN (R 4.2.0)
     utf8                     1.2.3    2023-01-31 [1] CRAN (R 4.2.3)
     vctrs                    0.6.0    2023-03-16 [1] CRAN (R 4.2.2)
     viridisLite              0.4.1    2022-08-22 [1] CRAN (R 4.2.3)
     vroom                    1.6.1    2023-01-22 [1] CRAN (R 4.2.2)
     withr                    2.5.0    2022-03-03 [1] CRAN (R 4.2.0)
     xfun                     0.37     2023-01-31 [1] CRAN (R 4.2.2)
     xtable                   1.8-4    2019-04-21 [2] CRAN (R 4.0.3)
     yaml                     2.3.7    2023-01-23 [1] CRAN (R 4.2.3)

     [1] /net/mulan/home/fredboe/R/x86_64-pc-linux-gnu-library/4.0
     [2] /net/mario/cluster/lib/R/site-library-bionic-40
     [3] /usr/local/lib/R/site-library
     [4] /usr/lib/R/site-library
     [5] /usr/lib/R/library

    ──────────────────────────────────────────────────────────────────────────────
