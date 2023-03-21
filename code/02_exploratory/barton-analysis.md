Exploring the creatinine GWAS summary statistics & FMD GWAS
meta-analysis summary statistics
================

We want to “explore” the two results files to see how many SNPs are
available in each, and to see how many SNPs are in both files. This
information will inform 2-sample MR studies to assess the causal effect
of FMD on creatinine levels.

First, we read into R the FMD summary statistics file.

``` r
#fmd_file <- here::here("data", "fmd_meta_gwas", 
#                        "meta_analyse_FMD_FUMA_FR_MAYO_DEFINE_POL_MGI_FEIRI_HRC_all_2020-08-12.tab")
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

`fmd_tib` has the summary statistics in a different format. It has 12
columns in total:

“MarkerName” “rsID”  
“CHROM”  
“POS”  
“REF”  
“ALT”  
“P”  
“BETA”  
“SE”  
“N”  
“Rsq_min”  
“Rsq_max”

``` r
library(magrittr)
```

``` r
# https://cran.r-project.org/web/packages/LDlinkR/vignettes/LDlinkR.html
# follow steps at above url to use LDlinkR to get LD matrix for use with MendelianRandomization R pkg
ld_token <- "3fbdb0673b22"
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
p_thresholds <- c(1e-08, 1e-07, 1e-06)
for (outcome in outcomes){
    for (p_threshold in p_thresholds){
#outcome <- outcomes[1]
#p_threshold <- p_thresholds[1]
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
        # make ld matrix
        ld_list <- list()
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
        # filter to remove highly correlated SNPs
        small_dat_no_ld <- small_dat %>%
            dplyr::filter(SNP %in% rownames(ld_mat))
        # make input object
        input <- MendelianRandomization::mr_input(
                    bx = small_dat_no_ld$BETA, 
                    bxse = small_dat_no_ld$SE, 
                    by = small_dat_no_ld$beta, 
                    byse = small_dat_no_ld$standard_error,
                    corr = ld_mat,
                    exposure = "FMD",
                    outcome = outcome,
                    snps = rownames(ld_mat)
                )
        # make header for collection of graphs for each iteration of loop
        cat('\n\n## `', outcome, "with p-value threshold: ", p_threshold, '`\n\n')    
        # MR analyses   
        print(MendelianRandomization::mr_allmethods(input))
        # MR plots
 #       fig_fn <- here::here("figures", "mr_plots", paste0(outcome, "_", p_threshold, "_1.png"))
 #       png(fig_fn)
 #       MendelianRandomization::mr_plot(input, interactive = FALSE, line = "ivw", labels = TRUE, orientate = TRUE)
 #       dev.off()
##        knitr::include_graphics(fig_fn, rel_path = FALSE)
 #       #
 #       fig_fn <- here::here("figures", "mr_plots", paste0(outcome, "_", p_threshold, "_2.png"))
 #       png(fig_fn)
 #       MendelianRandomization::mr_plot(input, interactive = FALSE, line = "egger", labels = TRUE, orientate = TRUE)
 #       dev.off()
##        knitr::include_graphics(fig_fn, rel_path = FALSE)
#
#        #
#        fig_fn <- here::here("figures", "mr_plots", paste0(outcome, "_", p_threshold, "_3.png"))
#        png(fig_fn)
#        MendelianRandomization::mr_plot(MendelianRandomization::mr_allmethods(input, method = "all"), 
#                                        interactive = FALSE, 
#                                        labels = TRUE, orientate = TRUE)
#        dev.off()
#        knitr::include_graphics(fig_fn, rel_path = FALSE)
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

    LDlink server is working...

## `CystatinC with p-value threshold:  1e-08`

                    Method Estimate Std Error 95% CI        P-value
             Simple median    0.006     0.005  -0.004 0.016   0.211
           Weighted median    0.006     0.005  -0.003 0.016   0.163

Penalized weighted median 0.007 0.004 -0.002 0.016 0.128

                       IVW   -0.001     0.004  -0.008 0.006   0.820
             Penalized IVW   -0.001     0.004  -0.008 0.006   0.820
                Robust IVW   -0.001     0.004  -0.008 0.006   0.820
      Penalized robust IVW   -0.001     0.004  -0.008 0.006   0.820
                                                                   
                  MR-Egger    0.073     0.061  -0.046 0.192   0.229
               (intercept)   -0.023     0.019  -0.060 0.014   0.219
        Penalized MR-Egger    0.073     0.061  -0.046 0.192   0.229
               (intercept)   -0.023     0.019  -0.060 0.014   0.219
           Robust MR-Egger    0.073     0.061  -0.046 0.192   0.229
               (intercept)   -0.023     0.019  -0.060 0.014   0.219

Penalized robust MR-Egger 0.073 0.061 -0.046 0.192 0.229 (intercept)
-0.023 0.019 -0.060 0.014 0.219

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    LDlink server is working...

## `CystatinC with p-value threshold:  1e-07`

                    Method Estimate Std Error 95% CI         P-value
             Simple median    0.007     0.004  -0.001  0.014   0.073
           Weighted median    0.007     0.004  -0.001  0.014   0.071

Penalized weighted median 0.008 0.004 0.000 0.015 0.042

                       IVW   -0.012     0.008  -0.028  0.004   0.139
             Penalized IVW   -0.012     0.008  -0.028  0.004   0.139
                Robust IVW   -0.012     0.008  -0.028  0.004   0.139
      Penalized robust IVW   -0.012     0.008  -0.028  0.004   0.139
                                                                    
                  MR-Egger    0.090     0.019   0.052  0.128   0.000
               (intercept)   -0.029     0.005  -0.039 -0.018   0.000
        Penalized MR-Egger    0.090     0.019   0.052  0.128   0.000
               (intercept)   -0.029     0.005  -0.039 -0.018   0.000
           Robust MR-Egger    0.090     0.019   0.052  0.128   0.000
               (intercept)   -0.029     0.005  -0.039 -0.018   0.000

Penalized robust MR-Egger 0.090 0.019 0.052 0.128 0.000 (intercept)
-0.029 0.005 -0.039 -0.018 0.000

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    LDlink server is working...

## `CystatinC with p-value threshold:  1e-06`

                    Method Estimate Std Error 95% CI         P-value
             Simple median   -0.012     0.004  -0.020 -0.003   0.006
           Weighted median    0.005     0.004  -0.003  0.013   0.224

Penalized weighted median 0.007 0.004 0.000 0.015 0.050

                       IVW   -0.027     0.004  -0.035 -0.019   0.000
             Penalized IVW   -0.027     0.004  -0.035 -0.019   0.000
                Robust IVW   -0.027     0.004  -0.035 -0.019   0.000
      Penalized robust IVW   -0.027     0.004  -0.035 -0.019   0.000
                                                                    
                  MR-Egger    0.067     0.013   0.041  0.093   0.000
               (intercept)   -0.023     0.003  -0.029 -0.016   0.000
        Penalized MR-Egger    0.067     0.013   0.041  0.093   0.000
               (intercept)   -0.023     0.003  -0.029 -0.016   0.000
           Robust MR-Egger    0.067     0.013   0.041  0.093   0.000
               (intercept)   -0.023     0.003  -0.029 -0.016   0.000

Penalized robust MR-Egger 0.067 0.013 0.041 0.093 0.000 (intercept)
-0.023 0.003 -0.029 -0.016 0.000

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    LDlink server is working...

## `Creatinine with p-value threshold:  1e-08`

                    Method Estimate Std Error 95% CI        P-value
             Simple median   -0.004     0.005  -0.014 0.007   0.472
           Weighted median   -0.003     0.005  -0.013 0.007   0.556

Penalized weighted median 0.000 0.005 -0.009 0.009 0.965

                       IVW   -0.007     0.004  -0.015 0.000   0.057
             Penalized IVW   -0.007     0.004  -0.015 0.000   0.057
                Robust IVW   -0.007     0.004  -0.015 0.000   0.057
      Penalized robust IVW   -0.007     0.004  -0.015 0.000   0.057
                                                                   
                  MR-Egger    0.047     0.076  -0.103 0.196   0.542
               (intercept)   -0.017     0.023  -0.063 0.029   0.475
        Penalized MR-Egger    0.047     0.076  -0.103 0.196   0.542
               (intercept)   -0.017     0.023  -0.063 0.029   0.475
           Robust MR-Egger    0.047     0.076  -0.103 0.196   0.542
               (intercept)   -0.017     0.023  -0.063 0.029   0.475

Penalized robust MR-Egger 0.047 0.076 -0.103 0.196 0.542 (intercept)
-0.017 0.023 -0.063 0.029 0.475

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    LDlink server is working...

## `Creatinine with p-value threshold:  1e-07`

                    Method Estimate Std Error 95% CI         P-value
             Simple median    0.001     0.004  -0.007  0.009   0.843
           Weighted median    0.001     0.004  -0.007  0.010   0.785

Penalized weighted median 0.002 0.005 -0.008 0.012 0.676

                       IVW   -0.018     0.010  -0.037  0.001   0.063
             Penalized IVW   -0.018     0.010  -0.037  0.001   0.063
                Robust IVW   -0.018     0.010  -0.037  0.001   0.063
      Penalized robust IVW   -0.018     0.010  -0.037  0.001   0.063
                                                                    
                  MR-Egger    0.098     0.031   0.038  0.159   0.001
               (intercept)   -0.033     0.009  -0.049 -0.016   0.000
        Penalized MR-Egger    0.098     0.031   0.038  0.159   0.001
               (intercept)   -0.033     0.009  -0.049 -0.016   0.000
           Robust MR-Egger    0.098     0.031   0.038  0.159   0.001
               (intercept)   -0.033     0.009  -0.049 -0.016   0.000

Penalized robust MR-Egger 0.098 0.031 0.038 0.159 0.001 (intercept)
-0.033 0.009 -0.049 -0.016 0.000

    Rows: 5515075 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): ALLELE1, ALLELE0
    dbl (13): chromosome, base_pair_location, GENPOS, A1FREQ, INFO, CHISQ_LINREG...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    LDlink server is working...

## `Creatinine with p-value threshold:  1e-06`

                    Method Estimate Std Error 95% CI         P-value
             Simple median   -0.018     0.005  -0.027 -0.008   0.000
           Weighted median   -0.004     0.005  -0.013  0.006   0.444

Penalized weighted median -0.002 0.005 -0.012 0.007 0.613

                       IVW   -0.039     0.005  -0.049 -0.029   0.000
             Penalized IVW   -0.039     0.005  -0.049 -0.029   0.000
                Robust IVW   -0.039     0.005  -0.049 -0.029   0.000
      Penalized robust IVW   -0.039     0.005  -0.049 -0.029   0.000
                                                                    
                  MR-Egger    0.083     0.020   0.044  0.122   0.000
               (intercept)   -0.029     0.005  -0.039 -0.020   0.000
        Penalized MR-Egger    0.083     0.020   0.044  0.122   0.000
               (intercept)   -0.029     0.005  -0.039 -0.020   0.000
           Robust MR-Egger    0.083     0.020   0.044  0.122   0.000
               (intercept)   -0.029     0.005  -0.039 -0.020   0.000

Penalized robust MR-Egger 0.083 0.020 0.044 0.122 0.000 (intercept)
-0.029 0.005 -0.039 -0.020 0.000
