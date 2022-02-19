#' Add __OOV__ if out of vocabulary
#'
#' Convert tokens inside `.list` in a "__OOV__" if their representation
#' as an integer in the dictionary is greater than the maximum allowed
#' one as set in `max_word`
#'
#' @param .list (list) tokens, i.e. named integers (each integer is the
#'   name's index in the dictionary), to considered
#' @param max_words (int) maximum index considered in the current
#'  representation
#'
#' @return a modified version of `.list` with index `max_word + 1`, named "__OOV__", for all the indeces exceeding `max_word`
#' @export
#'
#' @examples
#'
#' corpus <- list(
#'   a = setNames(seq_along(letters), letters),
#'   A = setNames(seq_along(letters), LETTERS)[1:10]
#' )
#' add_oov_when_greater_than(corpus, 20)
#'
add_oov_when_greater_than <- function(.list, max_words) {
    max_words <- as.integer(max_words)

    purrr::map(.list, ~{
        is_oov <- .x > max_words
        .x[is_oov] <- max_words + 1L
        names(.x)[is_oov] <- rep("__OOV__", sum(is_oov))
        .x
    })
}
