#' Create the main mixdb for the otiti project
#'
#' Create the main [mixdb] for the otiti project
#'
#' @param data_path (chr) path to the data folder
#' @param pedia_gold_path (chr) path to pedinet gold standard file RDS
#' @param output_path (chr) path to store the output mixdb in RDS format
#'
#' @return invisibly `TRUE``
#' @export
create_otiti_mixdb <- function(
    data_path = here::here("../data"),
    pedia_gold_path = file.path(data_path, "pedia_gold_otiti.rds"),
    output_path = file.path(data_path, "mixdb_otiti_tagged.rds")
) {
    gold_otiti <- readr::read_rds(pedia_gold_path) %>%
        dplyr::filter(!is.na(class))

    mixdb_otiti_tagged <- gold_otiti %>%
        dplyr::mutate_if(is.character, stringr::str_to_lower) %>%
        dplyr::mutate_if(is.character, tidyr::replace_na, "__NA__") %>%
        dplyr::mutate_if(is.character, code_num) %>%
        dplyr::mutate_if(is.character, expand_punctuations) %>%
        mixdb(meta_vars(
            set, id_medico, guidpaziente, datacontatto, oracontatto,
            data_nascita, sesso
        ))

    saveRDS(mixdb_otiti_tagged, output_path)
    invisible(TRUE)
}
