library(limpido)
library(tidyverse)
library(keras)
library(glue)
library(here)

perc_seq <- seq(0, 100, by = 20)
ensembles <- vector("list", length(perc_seq)) %>%
    set_names(paste0("perc_", perc_seq))

for (perc_vali in perc_seq) {

    to_load <- here("..", "..", "output", "output_ver3xxx") %>%
        list.files(
            glue("-{perc_vali}vali.rda$"),
            full.names = TRUE
        )

    model_names <- c("m1", "m2", "m3", "m4")

    for (i in seq_along(to_load)) {
        load(to_load[[i]])
        assign(model_names[[i]], unserialize_model(model_raw))

    }

    ensembles[[paste0("perc_", perc_vali)]] <- ensemble_predict(
        list(m1, m2, m3, m4),
        params
    )

    # ensemble <- ensembles[[paste0("perc_", 100)]]
    # save(ensemble, file = here(
    #     "..", "..", "output", "output_ensembles_ver3",
    #     glue("ensemble_{100}.rda")
    # ))
}

save(ensembles, file = here(
        "..", "..", "output", "output_ensembles_ver3",
        glue("ensembles.rda")
    ))



ensembles %>%
    imap_dfr(
        ~.x[["metrics"]] %>%
            as_tibble() %>%
            mutate(perc = .y)
) %>%
    pivot_longer(-perc) %>%
    rename(Metric = name) %>%
    separate(perc, c("name", "time")) %>%
    select(-name) %>%
    filter(str_detect(Metric, "balanced|accuracy")) %>%
    mutate(
        time = as.integer(time),
        Metric = str_replace_all(Metric, c(
            "crude_accuracy" = "Accuracy",
            "balanced_precision" = "Precision (balanced)",
            "balanced_rec" = "Recall (balanced)",
            "balanced_f" = "F1 (balanced)"
        ))
    ) %>%
    ggplot(aes(x = time, y = value, colour = Metric, linetype = Metric)) +
    geom_line() +
    xlab("% of validation included in the training") +
    ylab("Performance") +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(breaks = seq(0.8, 1, by = 0.02))



ggsave("ensemble_perf_curves.png", width = 8, height = 6)
