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
code_num <- function(text, code) {
    UseMethod("code_num", text)
}

#' @rdname code_num
#' @export
code_num.character <- function(text, code = "[NUM]") {
    stringr::str_replace_all(text, "\\d+", code)
}