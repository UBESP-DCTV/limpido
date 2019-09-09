#' Get metrix from trained objects
#'
#' given the path to the output folder in which the `.rda` file output
#' of the script in `limpido/analyses/R` are stored, provides useful
#' metrics from the model
#'
#' @param path (chr, default = "../output") output path for the stored models
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     get_metrics()
#' }
get_metrics <- function(path = here::here("../output/")) {

    filenames <- list.files(path, "\\.rda$")
    pb <- depigner::pb_len(length(filenames))


    purrr::map_df(filenames, ~{

        load(file.path(path, .x))


        name <- stringr::str_extract(.x, "-2\\d{2}-[^\\.]+") %>%
            stringr::str_remove("^-")

        finetuned <- stringr::str_detect(name, "fine_tuned")

        if (finetuned) {
            name <- stringr::str_remove(name, "-fine_tuned")
        }


        val_acc <- history[["metrics"]][["val_categorical_accuracy"]]
        if (params$is_test) {
            stoped_epoch <- length(history[["metrics"]][["loss"]])
        } else {
            # we need to consider the first epoch that reached the maximum
            # values of the validation accuracy
            stoped_epoch <- which.max(val_acc)
        }


        res <- data_frame(
            name = name,
            v_acc = 100 * val_acc[[stoped_epoch]] %>% round(4),
            epoch = stoped_epoch,
            tuned = as.character(finetuned),
            test = as.character(params$is_test)
        ) %>%
            bind_cols(
                100 *
                    as_tibble(preds$metrics) %>%
                    rename_all(~stringr::str_remove(., "^crude_")) %>%
                    rename_all(~stringr::str_replace(., "^balanced", "bal")) %>%
                    round(4)
            ) %>%
        mutate(check = v_acc == accuracy)

        depigner::tick(pb, name)
        res

    })

}
