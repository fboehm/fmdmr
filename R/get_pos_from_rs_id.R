#' Get GRCh38 position AND GRCh37 for lead SNP
#' @param rs_id a character vector of length one, ie, one SNP rs id value for one lead SNP
#' @return a tibble with one row and columns for both GRCh37 and GRCh38 positions
#' @details queries biomaRt API twice per SNP
#' @export
get_pos_from_rs_id <- function(rs_id){
    # get rsid from chromosome and position
    dataset <- "hsapiens_snp"
    attributes <- c("refsnp_id", "chr_name", "chrom_start", "chrom_end")
    # create a biomaRt object for the GRCh37 reference genome
    mart <- biomaRt::useMart(biomart = "ENSEMBL_MART_SNP", dataset = dataset)
    # retrieve the SNP information using biomaRt
    filters <- list(snp_filter = rs_id) 
    result1 <- biomaRt::getBM(attributes = attributes, filters = filters, mart = mart) %>%
        dplyr::rename(pos_grch38 = chrom_start) 
    # now GRCh37
    host <- "https://grch37.ensembl.org"
    mart <- biomaRt::useMart(biomart = "ENSEMBL_MART_SNP", dataset = dataset, host = host)
    # retrieve the SNP information using biomaRt
    result2 <- biomaRt::getBM(attributes = attributes, filters = filters, mart = mart) %>%
        dplyr::rename(pos_grch37 = chrom_start)
    result <- result1 %>%
        dplyr::inner_join(result2, by = c("refsnp_id", "chr_name")) %>%
        dplyr::select( - chrom_end.x, - chrom_end.y)
    return(result)
}
