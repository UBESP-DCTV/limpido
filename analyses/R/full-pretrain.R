library(limpido)
library(tidyverse)
library(furrr) # to load after purrr/tidyverse
    plan(multicore, workers = availableCores())
library(keras) # to load after furrr
library(lubridate)
library(here)
library(glue)

load(here("../../data-raw/dataset_final.rda"))

mixdb_pedianet_long <- dataset_final %>%
    pivot_longer(
        cols = -c(guidpaziente, datacontatto, oracontatto, sesso,
                  data_nascita,idmedico),
        values_drop_na = TRUE
    ) %>%
    mutate(class = 0L) %>%
    mixdb(meta_vars(guidpaziente, datacontatto, oracontatto, sesso,
                    data_nascita,idmedico, name))


saveRDS(mixdb_pedianet_long, here::here("../../data/mixdb_pedianet_long.RDS"))
