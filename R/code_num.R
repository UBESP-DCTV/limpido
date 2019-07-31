#' Code numbers
#'
#' @param text (chr) vector of text to in which code the numbers
#' @param code (chr) code to use to substitute the numbers
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
    code = "[NUM]",
    ignore_in_word = FALSE
) {
    if (ignore_in_word) {
        match <- "(^.{0}|\\W)(\\d+)(\\W|.{0}$)"
        replacement <- paste0("\\1", code, "\\3")
    } else {
        match <- "\\d+"
        replacement <- code
    }
    stringr::str_replace_all(text, match, replacement)
}
