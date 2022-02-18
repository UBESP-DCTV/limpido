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

#' @describeIn expand_punctuations method for characters
#' @export
expand_punctuations.character <- function(text) {

    # the aim is to shrink punctuation repetitions

    regex <- "(\\W)\\1*"
    ui_done("[1/4] regex")

    # avoid shrink on elipsis
    text <- stringr::str_replace_all(text, "\\.{3}", " __ELIPSIS__ ")
    ui_done("[2\4] elipsis")

    # add spaces arrouond (single occurences of) punctuations
    text <- stringr::str_replace_all(text, regex, " \\1 ")
    ui_done("[3\4] separate punctuation")

    # remove extra spaces
    text <- stringr::str_squish(text)
    ui_done("[4/4] squish")
    text
}
