#' Get LD values for a collection of SNPs on one chromosome
#' 
#' @param snps a character vector of SNPs
#' @param genome_build a character string of the genome build to use for the LDlinkR query
#' @param token a character string of the LDlinkR token
#' @param pop a character string of the population to use for the LDlinkR query
#' @return a tibble with 3 columns: row_name, column_name, value
#' @details This function is used to get LD values for a collection of SNPs.
#' @export 
get_ld_df <- function(snps, genome_build, token, pop = "CEU"){
    stopifnot(length(snps) <= 1000)
    stopifnot(pop %in% c("CEU", "YRI", "CHB", "JPT"))
    stopifnot(genome_build %in% c("grch37", "grch38"))
    out <- LDlinkR::LDmatrix(snps, pop = pop, token = token, genome_build = genome_build)
    return(out)
}

#' Convert ld_df to a R matrix
#' 
#' @param ld_df typically the output from get_ld_df
#' @return a square R matrix containing LD values
#' @export 
convert_df_to_matrix <- function(ld_df){
    mat <- as.matrix(ld_df[, -1])
    rownames(mat) <- colnames(mat)
    return(mat)
}