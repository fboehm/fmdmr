

library(BSgenome)

library(VariantAnnotation)
vcf <- readVcf("data/dbSNP_vcf.gz")
gr <- as(granges(vcf), "GRanges")

bsgenome <- BSgenome::BSgenomeFromGRanges(gr)
library(SNPlocs.Hsapiens.dbSNP144.GRCh37)



