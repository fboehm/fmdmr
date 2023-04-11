#' Parse correlated alleles
#' 
#' @param corr_alleles a character vector of correlated alleles, typically outputted from LDlinkR::LDmatrix

parse_correlated_alleles <- function(corr_alleles){
    # parse correlated alleles
    # example: "rs1(A/G),rs2(A/G),rs3(A/G)"
    # output: list of tibbles
    # each tibble has 3 columns: refsnp_id, allele1, allele2
    # each tibble has 1 row per cor
        
}