#' Code numbers
#'
#' @param text (chr) vector of text to in which code the numbers
#' @param code (chr) code to use to substitute the numbers
#' @param ignore_in_word (lgl, default FALSE) should digits into words
#'     be ignored?
#'
#' @return (chr) vector of the same length of `text` with al the numbers
#'     changed in `code`
#' @export
#'
#' @examples
#' code_num(c("1", "12", "abc12", "abc.12", "l'82"))
code_num <- function(text, code, ignore_in_word) {
    UseMethod("code_num", text)
}

#' @rdname code_num
#' @export
code_num.character <- function(
    text,
    code = "__NUM__",
    ignore_in_word = FALSE
) {
    if (ignore_in_word) {
        match <- "(^.{0}|\\W)(\\d+)(\\.\\d+)*(\\W|.{0}$)"
        replacement <- paste0("\\1", code, "\\4")
    } else {
        match <- "\\d+(\\.\\d+)*"
        replacement <- code
    }
    stringr::str_replace_all(text, match, replacement)
}
