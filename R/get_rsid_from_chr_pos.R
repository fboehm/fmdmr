#' Get rs_id from chromosome and position
#' 
#' @param chr a character vector of length 1- the chromosome
#' @param pos a numeric vector of snp positions on the chromosome chr
#' @param host a character string of the host to use for the biomaRt query
#' @return a tibble with one row per SNP
#' @details queries biomaRt API once per SNP
#' @export
get_rsid_from_chr_pos <- function(chr, pos, host = "https://grch37.ensembl.org"){
    # get rsid from chromosome and position
    dataset <- "hsapiens_snp"
    attributes <- c("refsnp_id", "chr_name", "chrom_start", "chrom_end")
    # create a biomaRt object for the GRCh37 reference genome
    mart <- biomaRt::useMart(biomart = "ENSEMBL_MART_SNP", dataset = dataset, host = host)
    # retrieve the SNP information using biomaRt
    results <- list()
    for (pp in pos){
        filters <- list(chr_name = as.character(chr), start = pp, end = pp) # to return all snps between start and end
        results[[as.character(pp)]] <- biomaRt::getBM(attributes = attributes, filters = filters, mart = mart) %>%
            tibble::as_tibble() %>%
            dplyr::filter(chrom_start == chrom_end) %>%
            dplyr::filter(stringr::str_starts(refsnp_id, "rs"))
        cat("Retrieved SNP information for position ", pp, " on chromosome ", chr, "\n")
    }
    tib <- purrr::discard(results, ~nrow(.x) == 0) %>%
        dplyr::bind_rows()
    return(tib)
}
