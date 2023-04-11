#' Parse ldlink output for proxy SNPs' alleles in column called Correlated_Alleles
#' 
#' @param allele_string a character string of alleles from the Correlated_Alleles column with form like A=A,G=C
#' @details lead SNP's alleles are always on the left side of the equals sign in the Correlated_Alleles column
#' @return a tibble with two rows per proxy SNP, one row per allele
#' @export
#' @examples
#' a_string <- "A=A,G=C"
#' parse_correlated_alleles(a_string)
parse_correlated_alleles <- function(allele_string){
    two_with_equals <- stringr::str_split(allele_string, pattern = ",") %>%
        unlist() 
    result <- two_with_equals %>%
        stringr::str_split(pattern = "=") %>%
        unlist() %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::rename(lead_allele = V1, proxy_allele = V2)
    return(result)
}

#' 
