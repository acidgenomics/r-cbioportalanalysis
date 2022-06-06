test_that("rnaSeqData", {
    geneNames <- c("MYC", "TP53")
    cancerStudy <- "ccle_broad_2019"
    x <- rnaSeqData(
        cancerStudy = cancerStudy,
        geneNames = geneNames
    )
    expect_s4_class(x, "SummarizedExperiment")
    expect_identical(rownames(x), geneNames)
    expect_identical(colnames(x)[[1L]], "A101D_SKIN")
})

