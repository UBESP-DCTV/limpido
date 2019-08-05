paste_sep <- function(x, y) purrr::map2(x, y, ~c(.x, "[SEP]", .y))
