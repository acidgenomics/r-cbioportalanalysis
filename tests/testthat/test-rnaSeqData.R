test_that("rnaSeqData", {
    geneNames <- c("MYC", "TP53")
    cancerStudy <- "ccle_broad_2019"
    x <- rnaSeqData(cancerStudy = cancerStudy, geneNames = geneNames)
    expect_s4_class(x, "SummarizedExperiment")
})

