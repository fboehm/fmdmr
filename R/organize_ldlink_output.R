#' Organize outputted LD matrix (a data.frame) from LDlink into 3-column tibble & discard NAs 
#' 
#' @param ldlink_output a data.frame from LDlinkR::LDmatrix
#' @return a tibble with 3 columns: row_name, column_name, value
#' @export
organize_ldlink_output <- function(ldlink_output){
    out <- ldlink_output %>% 
        dplyr::rename(row_name = RS_number) %>% # column RS_number is hardcoded because it's always in the LDlinkR output
        tidyr::gather(column_name, value, - row_name) %>%
        dplyr::filter(stringr::str_starts(string = column_name, pattern = "rs")) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(dplyr::desc(value)) 
    return(out)
}

