#' Calculate genetic correlation between a pair of traits from GWAS summary statistics
#' 
#' @param dat a harmonised data set, typically from \code{TwoSampleMR::harmonise}
#' @param snpinfo typically the output of \code{ieugwasr::afl2_list("hapmap3")}
#' @ref https://github.com/MRCIEU/TwoSampleMR/blob/master/R/ldsc.r
#' @return a list with four components: \code{gcov} the genetic covariance, \code{h1} the heritability of the first trait, \code{h2} the heritability of the second trait, and \code{rg} the genetic correlation
#' @export

ldsc_rg2 <- function(dat, snpinfo = ieugwasr::afl2_list("hapmap3"))
{
    stopifnot(nrow(dat) > 0)
    h1 <- TwoSampleMR:::ldsc_h2_internal(d1$z1, d1$l2, d1$n1, d1$l2)
    h2 <- TwoSampleMR:::ldsc_h2_internal(d2$z2, d2$l2, d2$n2, d1$l2)
    dat <- dplyr::inner_join(d1, d2, by="rsid") %>%
        dplyr::mutate(
            l2 = l2.x,
            n1 = as.numeric(n1),
            n2 = as.numeric(n2),
            rhs = l2 * sqrt(n1 * n2)
        )

    gcov <- dat %>%
        {
            ldsc_rg_internal(
                Zs = cbind(.$z1, .$z2),
                r2 = .$l2,
                h1 = h1$coefficients[2,1] * nrow(d1),
                h2 = h2$coefficients[2,1] * nrow(d2),
                N1 = .$n1,
                N2 = .$n2,
                W = .$l2
            )
        }
    return(list(
        gcov = gcov,
        h1=h1,
        h2=h2,
        rg = (gcov$coefficients[1,1] * nrow(dat)) / sqrt(h1$coefficients[2,1] * nrow(d1) * h2$coefficients[2,1] * nrow(d2))
    ))
}
