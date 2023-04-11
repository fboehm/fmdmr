#' Get SNPs in high LD with each lead SNP
#' @param snps a character vector of SNPs - either rsids or chromosome:position
#' @param genome_build a character string of the genome build
#' @param ld_token a character string of the LDlinkR token
#' @param pop a character string of the population to use for LD calculation
#' @param r2_threshold a numeric value of the r2 threshold to use in filtering correlated SNPs
#' @return a tibble with one row per correlated SNP
#' @details queries LDlink API once per lead SNP

get_correlated_snps <- function(snps, genome_build, ld_token, pop = "CEU", r2_threshold = 0.8){
    ldlink_output <- list()
    for (snp in snps){
        ldlink_output[[snp]] <- LDlinkR::LDproxy(snp = snp, genome_build = genome_build, 
                                                 token = ld_token, pop = pop) %>%
            tibble::as_tibble() %>%
            dplyr::filter(R2 >= r2_threshold) %>%
            dplyr::mutate(lead_snp = snp)
    }
    result <- ldlink_output %>%
        dplyr::bind_rows()
    return(result)
}
