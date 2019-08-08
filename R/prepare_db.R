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
    on.exit(usethis::ui_done("DB preparation finished."))


    only_text <- final_pedia_db %>%
        dplyr::select(diagnosi1:risultato_8) %>%
        dplyr::mutate_all(stringr::str_to_lower) %>%
        tidyr::gather() %>%
        dplyr::distinct() %>%
        `[[`("value") %>%
        `[`(!is.na(.))
    usethis::ui_done("Textual vector ready")

    res <- code_num(only_text)
    usethis::ui_done("Numbers coded")

    expand_punctuations(res)
}
