#' expand punctuation
#'
#' @param text (chr) vector
#'
#' @return a character vector with all punctuation, sourrendered by
#'    spaces
#' @export
#'
#' @examples
#' expand_punctuations(c("abc de", "a.b", "A.B", "c'è", "s.p.a."))
expand_punctuations <- function(text) {
    UseMethod("expand_punctuations", text)
}

#' @rdname expand_punctuations
#' @export
expand_punctuations.character <- function(text) {

    # the aim is to shrink punctuation repetitions

    regex <- paste0("(\\W)\\1*")


    text %>%
        # avoid shrink on elipsis
        stringr::str_replace_all("\\.{3}", " __ELIPSIS__ ") %>%

        # add spaces arrouond (single occurences of) punctuations
        stringr::str_replace_all(regex, " \\1 ") %>%

        # remove extra spaces
        stringr::str_squish()
}
