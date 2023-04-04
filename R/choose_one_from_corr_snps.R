#' Choose quasi-independent SNPs from among correlated SNPs by using exposure association p-values and LD thresholding
#' 
#' @param snp_info a tibble with one row per SNP and columns "refsnp_id", "chr_name", "pos", "p_value".
#' @param ld_tibble a tibble with 3 columns: row_name, column_name, value. Typically the output from LDlinkR::LDmatrix and organize_ldlink_output
#' @param ld_threshold a numeric value between 0 and 1. The threshold for pairwise LD values to consider two SNPs correlated.
#' @return a tibble with one row per quasi-independent SNP and columns "refsnp_id", "chr_name", "pos", "p_value"
#' @details This function is used to choose a proxy SNP from among correlated SNPs by using exposure association p-values.
#' @export
#' @examples
#' snp_info <- tibble::tibble(refsnp_id = c("rs1", "rs2", "rs3"), chr_name = c("1", "1", "1"), 
#'      pos = c(100, 200, 300), p_value = c(0.01, 0.02, 0.03)) 
#' ld_threshold = 0.5
#' ld_tibble <- tibble::tibble(row_name = rep(c("rs1", "rs2", "rs3"), each = 3), 
#'  column_name = rep(c("rs1", "rs2", "rs3"), times = 3),
#'  value = c(1, 0.6, 0.2, 0.6, 1, 0.3, 0.2, 0.3, 1))
#' choose_one_from_corr_snps(snp_info, ld_tibble, ld_threshold)
#'  
choose_one_from_corr_snps <- function(snp_info, ld_tibble, ld_threshold = 0.5){
    # define correlated SNP clusters
    # then choose the snp with smallest p-value for each cluster
    # choose a row from snp_info then id its correlated SNPs
    out <- list()
    snps_in_ld <- c()
    for (row_num in seq_len(nrow(snp_info))){
        ss <- snp_info %>%
            dplyr::slice(row_num) 
        if (ss$refsnp_id %in% snps_in_ld){
            next
        }
        lt <- ld_tibble %>%
            dplyr::filter(row_name == ss$refsnp_id) %>%
            dplyr::filter(value >= ld_threshold) 
        snps_in_ld <- c(unique(c(lt$row_name, lt$column_name)), snps_in_ld)
        si <- snp_info %>%
            dplyr::filter(refsnp_id %in% snps_in_ld) %>%
            dplyr::arrange(p_value) %>%
            dplyr::slice(1)
        out[[as.character(si$refsnp_id)]] <- si
    }
    result <- purrr::discard(out, ~nrow(.x) == 0) %>%
        dplyr::bind_rows()
    return(result)
}
