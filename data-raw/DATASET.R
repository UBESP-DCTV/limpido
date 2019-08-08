## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(readxl)
library(lubridate)
library(here)
library(janitor)



validation_08_17 <- here::here(
    '../../data-raw/only_validation_otiti_08_17_funzionante_QC.xlsx'
) %>%
    read_xlsx() %>%
    select(-year) %>%
    rename(
        id_medico = idmedico,
        class = classe
    ) %>%
    mutate(
        datacontatto = as.Date(datacontatto),
        data_nascita = as.Date(data_nascita),
        oracontatto = (as.numeric(oracontatto) + 2209075200) %>%
            structure(units = "secs", class = c("hms", "difftime"))
    )

save(validation_08_17,
    file = here::here('../../data/validation_08_17.rda')
)



load(here('../../data/dataset_final.rda'))
load(here("../../data/gold_04_07.rda"))
load(here("../../data/validation_08_17.rda"))


class_train <- gold_04_07 %>%
    select(
        class, guidpaziente, datacontatto, oracontatto, id_medico
    ) %>%
    mutate(set = "train")


class_valid <- validation_08_17 %>%
    select(
        class, guidpaziente, datacontatto, oracontatto,  id_medico
    ) %>%
    mutate(set = "validation")

class_train_valid <- bind_rows(class_train, class_valid)

dataset_final2 <- dataset_final %>%
    rename(id_medico = idmedico) %>%
    mutate(
        sesso = stringr::str_to_lower(sesso),
        guidpaziente = stringr::str_to_lower(guidpaziente)
    ) %>%
    distinct(guidpaziente, datacontatto, oracontatto, id_medico,
        .keep_all = TRUE
    )

pedia_gold_otiti <- full_join(class_train_valid, dataset_final2)

saveRDS(pedia_gold_otiti,
    file = here('../../data/pedia_gold_otiti.rds'),
    compress = "xz"
)
