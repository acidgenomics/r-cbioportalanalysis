test_that("geneticProfiles", {
    x <- geneticProfiles(cancerStudy = "ccle_broad_2019")
    expect_s4_class(x, "DFrame")
    expect_identical(
        object = rownames(x)[[1L]],
        expected = "ccle_broad_2019_CCLE_drug_treatment_AUC"
    )
})
