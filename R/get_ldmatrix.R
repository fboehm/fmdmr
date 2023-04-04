#' Get LD values for a collection of SNPs on one chromosome
#' 
#' @param snps a character vector of SNPs
#' @param host a character string of the host to use for the LDlinkR query
#' @return a tibble with 3 columns: row_name, column_name, value
#' @details This function is used to get LD values for a collection of SNPs.
#' @export 
get_ld_tib <- function(snps, genome_build, token, pop = "CEU"){
    stopifnot(length(snps) <= 1000)
    stopifnot(pop %in% c("CEU", "YRI", "CHB", "JPT"))
    stopifnot(genome_build %in% c("grch37", "grch38"))
    out <- LDlinkR::LDmatrix(snps, pop = pop, token = token, genome_build = genome_build)
    result <- organize_ldlink_output(out)
    return(result)
}
