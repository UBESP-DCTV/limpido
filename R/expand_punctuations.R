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
expand_punctuations.character <- function(text, regex = "\\W+") {
    regex <- paste0("(", regex, ")")

    stringr::str_replace_all(text, regex, " \\1 ") %>%
        stringr::str_squish()
}
