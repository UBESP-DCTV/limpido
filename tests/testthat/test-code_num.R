test_that("coding numbers works", {
    reference <- c("1", "12", "abc 12", "abc12", "abc.12", "l'82")
    target <- c(
        "[NUM]", "[NUM]", "abc [NUM]", "abc[NUM]", "abc.[NUM]",
        "l'[NUM]"
    )

    expect_equal(code_num(reference), target)
})
