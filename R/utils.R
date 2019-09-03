paste_sep <- function(x, y) purrr::map2(x, y, ~c(.x, "__SEP__", .y))


onehot_to_label <- function(x) {
    tibble::as_tibble(t(x)) %>%
        purrr::map_int(which.max) %>%
        unname()
}
