#' Combine dictionaries
#'
#' @param dict_1 a \code{\link{dictionary}}
#' @param dict_2 a \code{\link{dictionary}}
#'
#' @return a \code{\link{dictionary}}
#' @seealso \code{\link{dictionary}}
#' @export
#'
#' @examples
#' dict_1 <- dictionary(c("a", "b", "b"))
#' dict_2 <- dictionary(c("a", "b", "c", "c", "c", "c"))
#' c_dict(dict_1, dict_2)
c_dict <- function(dict_1, dict_2) {

    stopifnot(inherits(dict_1, "dictionary"))
    stopifnot(inherits(dict_2, "dictionary"))

    common <- intersect(names(dict_1), names(dict_2))
    common_in_1 <- dict_1[common]
    common_in_2 <- dict_2[common]

    stopifnot(all(
        names(dict_1[common_in_1]) == names(dict_2[common_in_2])
    ))

    freq_1 <- get_frequencies(dict_1)
    freq_2 <- get_frequencies(dict_2)
    dict_freq <- if (length(common) == 0) {
        sort(c(freq_1, freq_2), decreasing = TRUE)
    } else {
        freq_start <- freq_1[!names(freq_1) %in% common]
        freq_end   <- freq_2[!names(freq_2) %in% common]

        freq_mid   <- (freq_1[common] + freq_2[common]) %>%
            purrr::set_names(common)

        sort(c(freq_start, freq_mid, freq_end), decreasing = TRUE)
    }

    structure(
        purrr::set_names(seq_along(dict_freq), names(dict_freq)),
        frequencies = dict_freq,
        class = "dictionary"
    )

}
