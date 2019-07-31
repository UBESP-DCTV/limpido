test_that("expansion works", {
    sample_text <- c("abc de", "a.b", "A.B", "c'è", "s.p.a.")
    target <- c("abc de", "a . b", "A . B", "c ' è", "s . p . a .")
    expect_equal(
        expand_punctuations(sample_text),
        target
    )
})

