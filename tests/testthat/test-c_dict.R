test_that("c_dict works", {
    dict_1 <- dictionary(c("a", "b", "b"))
    dict_2 <- dictionary(c("a", "b", "c", "c", "c", "c"))
    merged <- c_dict(dict_1, dict_2)

    expect_is(merged, "dictionary")
    expect_equal(names(merged), c("c", "b", "a"))
    expect_equivalent(unclass(merged), 1:3)
    expect_equal(
        get_frequencies(merged),
        c("c" = 4L, "b" = 3L, "a" = 2L)
    )
})
