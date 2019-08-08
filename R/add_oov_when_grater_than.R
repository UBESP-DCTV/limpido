add_oov_when_grater_than <- function(.list, max_words) {
    purrr::map(.list, ~{
        is_oov <- .x > max_words
        .x[is_oov] <- max_words + 1L
        names(.x[is_oov]) <- rep("__OOV__", sum(is_oov))
        .x
    })
}
