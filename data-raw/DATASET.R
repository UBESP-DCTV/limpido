## code to prepare `DATASET` dataset goes here

library(tidyverse)
# library(lubridate)
library(here)
# library(janitor)


here('../data-raw/dataset_final.rda') %>%
    load()

dataset_final %>%
    head() %>%
    mutate_if(is.character, str_to_lower, locale = "it_IT") %>%
    pivot_longer(diagnosi1:diagnosi3,
                 names_to = c(".value", "set"),
                 names_pattern = "(.*)(\\d$)"
    )


usethis::use_data("DATASET")
