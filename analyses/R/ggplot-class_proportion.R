pedia_gold_otiti %>%
    select(class, set) %>%
    remove_missing() %>%
    mutate(
        class = factor(class,
            levels = c(0, 1, 2, 3, 4, 5),
            labels = c(
                "not-otitis", "not-acute", "not-media", "OMA",
                "perforation", "recurrent"
            )
        )
    ) %>%
    group_by(set, class) %>%
    summarize(n = n()) %>%
    mutate(
        tot = sum(n),
        prop = n/tot
    ) %>%
    ggplot(aes(x = class, y = prop, fill = set)) +
    geom_bar(position = "dodge", stat = "identity") +
    ylab("proportion within sets") +
    theme_bw() +
    ggtitle(
        "Proportion of classes on the train, validation, and test sets"
    )
