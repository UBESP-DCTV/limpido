test_that("text_no_meta throw an error on wrong meta", {
    expect_error(text_no_meta(test_db, gender))
    expect_error(text_no_meta(test_db, 2))
})

test_that("text no meta return the correct object", {
    expect_is(text_no_meta(test_db), "tbl_df")

    expect_equal(
        text_no_meta(test_db),
        dplyr::select(test_db, notes, gender)
    )

    expect_equal(
        text_no_meta(test_db, meta_vars(gender)),
        dplyr::select(test_db, notes)
    )

    expect_equal(
        text_no_meta(test_db, meta_vars(id, gender)),
        dplyr::select(test_db, notes)
    )
})
