test_that("coding numbers works", {
    reference <- c("1", "12", "abc 12", "abc12", "abc.12", "l'82")
    target <- c(
        "[NUM]", "[NUM]", "abc [NUM]", "abc[NUM]", "abc.[NUM]",
        "l'[NUM]"
    )

    expect_equal(code_num(reference), target)
})

test_that("can admit words with numbers", {
    expect_equal(code_num("a2b1"), "a[NUM]b[NUM]")
    expect_equal(
        code_num("34 a2b1 12", ignore_in_word = TRUE),
        "[NUM] a2b1 [NUM]"
    )
})
