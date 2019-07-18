test_that("frequencies extractor works", {
    expect_equal(
        get_frequencies(test_mixdb_dictionary),
        purrr::set_names(
            c(3, 2, 1, 1, 1),
            c("notes", "foo", "annotated", "another", "bar")
        )
    )
})
