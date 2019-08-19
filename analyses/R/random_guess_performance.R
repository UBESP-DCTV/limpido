random_guess_accuracy <- function(params, n) {

    samples <- nrow(params$train_x)
    classes <- seq_len(params$n_class)

    dense_y <- t(params$train_y) %>%
        as_tibble() %>%
        map_int(~which(. == 1L))

    sampling_guesses <- function() {
        sample(classes, samples,
            replace = TRUE,
            prob = params$train_y %>% {colSums(.)/sum(.)}
        )
    }

    random_guesses <- map(seq_len(n), ~sampling_guesses())

    map_dbl(random_guesses, ~sum(. == dense_y))/samples
}


random_guess_accuracy(params, 1e4) %>% hist()
