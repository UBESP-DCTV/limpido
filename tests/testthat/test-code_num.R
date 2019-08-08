reference <- c("1", "12", "abc 12", "abc12", "abc.12", "l'82",
                   "12.3", "a2b1", "34 a2b1 12")

test_that("coding numbers works", {
    target_default <- c(
        "__NUM__", "__NUM__", "abc __NUM__", "abc__NUM__", "abc.__NUM__",
        "l'__NUM__", "__NUM__", "a__NUM__b__NUM__",
        "__NUM__ a__NUM__b__NUM__ __NUM__"
    )

    expect_equal(code_num(reference), target_default)
})

test_that("can admit words with numbers", {
    target_ignore <- c(
        "__NUM__", "__NUM__", "abc __NUM__", "abc12", "abc.__NUM__",
        "l'__NUM__", "__NUM__", "a2b1", "__NUM__ a2b1 __NUM__"
    )

    expect_equal(
        code_num(reference, ignore_in_word = TRUE),
        target_ignore
    )
})
