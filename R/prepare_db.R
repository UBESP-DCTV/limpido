#' DB preparation
#'
#' perform vectorization of all the pertinent columns in a single
#' vector of texts.
#'
#' @param final_pedia_db dataframe with the final version of
#'     the full pedianet db.
#'
#' @return (chr) Character vector with each element a cell of the
#'     `final_pedia_db` preprocessed
#' @export
#'
#' @examples
#' \dontrun{
#'     expanded <- prepare_db(dataset_final)
#' }
prepare_db <- function(final_pedia_db) {
    usethis::ui_info(
        "DB preparation: vectorise, __NUM__ codes, expand punctuation."
    )


    only_text <- final_pedia_db %>%
        dplyr::select(diagnosi1:risultato_8) %>%
        dplyr::mutate_all(stringr::str_to_lower)
    ui_done("lowered")


    only_text <- dplyr::mutate_all(only_text,
        tidyr::replace_na, "__NA__")
    ui_done("NA to __NA__")


    only_text <- tidyr::gather(only_text) %>%
        dplyr::distinct() %>%
        `[[`("value") %>%
        `[`(!is.na(.))
    ui_done("Single vector ready")

    only_text <- stringr::str_replace_all(only_text,
        "(^.*$)", "__SEP__ \\1 __SEP__"
    )
    ui_done("__SEP__ added")

    ui_done("Full textual vector ready")

    ui_info("Coding numbers...")
    res <- code_num(only_text)
    ui_done("Numbers coded")

    ui_info("Expanding punctuations...")
    res <- expand_punctuations(res)
    ui_done("Punctuations expanded")
    res
}
