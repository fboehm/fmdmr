
#' Get SNPs near a lead SNP and in a tibble of SNPs, like those from GWAS summary statistics text files
#' @param lead_snp_pos an integer vector of length one
#' @param lead_snp_chrom an integer vector of length one to indicate which numeric chromosome SNP is on
#' @param dat a tibble of SNPs, like those from GWAS summary statistics text files
#' @param dat_chrom_var the name of the variable in dat that indicates which chromosome the SNP is on
#' @param dat_pos_var the name of the variable in dat that indicates the position of the SNP
#' @param dist the distance in base pairs to search for SNPs near the lead SNP
#' @return a tibble of SNPs near the lead SNP
#' @export
get_snps_near_lead <- function(lead_snp_pos, lead_snp_chrom, dat, dat_chrom_var, dat_pos_var, dist = 100000){
    out <- dat %>%
        dplyr::filter({{ dat_chrom_var }} == lead_snp_chrom) %>%
        dplyr::filter( {{ dat_pos_var }} >= lead_snp_pos - dist & {{ dat_pos_var }} <= lead_snp_pos + dist) 
    return(out)
}
