test_list_dictionary <- dictionary(list("a", "b", "b"))
test_freq <- get_frequencies(test_list_dictionary)

test_that("dictionary return the correct class and attribute", {
    expect_is(test_list_dictionary, "dictionary")
    expect_is(test_freq, "integer")
    expect_identical(
        test_freq,
        sort(test_freq, decreasing = TRUE)
    )
    expect_equivalent(test_freq, c(2L, 1L))
})


test_that("dictionary return correct output", {
    expect_equivalent(unclass(test_list_dictionary), c(1L, 2L))
    expect_equivalent(names(test_list_dictionary), c("b", "a"))
})
