library(limpido)
library(tidyverse)
library(furrr) # to load after purrr/tidyverse
plan(multicore, workers = availableCores())
library(keras) # to load after furrr
library(lubridate)
library(here)
library(glue)

load(here("../../data/gold_04_07.rda"))


mixdb_otiti <- gold_04_07 %>%
    mutate(guidpaziente = as.factor(guidpaziente)) %>%
    # HERE WE SEPARATE PUNCTUATIONS AND ADD TAGS FOR NUMBERS
    mutate_if(is.character, code_num) %>%
    mutate_if(is.character, expand_punctuations) %>%
    mixdb(meta_vars(
        guidpaziente, datacontatto, oracontatto, sesso, data_nascita,
        random_id, id_medico, descrizione_1, risultato_1, descrizione_2,
        risultato_2, descrizione_3, risultato_3, descrizione_4,
        risultato_4, descrizione_5, risultato_5, descrizione_6,
        risultato_6, descrizione_7, risultato_7, descrizione_8,
        risultato_8
    ))


# Parameters --------------------------------------------------------------
ngram_range <- 2L

cache_ngram_path <- here("outputs/mixdb_otiti_ngram.rda")
if (file.exists(cache_ngram_path)) {
    load(cache_ngram_path)
} else {
    mixdb_otiti_ngram <- add_ngram(mixdb_otiti, ngram_range)
    save(mixdb_otiti_ngram, file = cache_ngram_path)
}

admitted_indeces <- seq_along(mixdb_otiti_ngram$x)[-c(1728, 3318)]
n_train <- 4600
max_features <- 54395L
maxlen <- 200L
batch_size <- 8L
embedding_dims <- 200L
epochs <- 15L

random_seed <- sample.int(1e4, 1)
set.seed(random_seed)
train_indeces <- sample(x = admitted_indeces, size = n_train)
tuning_indeces <- admitted_indeces[-train_indeces]

max_features <- min(max_features, max(get_dictionary(mixdb_otiti_ngram))) + 1L

# Train sequences
train_x <- mixdb_otiti_ngram$x[train_indeces] # completamente NA!!!!!!!!!1
keep_train <- map(train_x, ~.x < max_features)
train_x <- map2(train_x, keep_train, ~.x[.y])
train_y <- as.integer(mixdb_otiti_ngram$y[train_indeces])
range(train_x)

tuning_x <- mixdb_otiti_ngram$x[tuning_indeces]
keep_tuning <- map(tuning_x, ~.x < max_features)
tuning_x <- map2(tuning_x, keep_tuning, ~.x[.y])
tuning_y <- as.integer(mixdb_otiti_ngram$y[tuning_indeces])
range(tuning_x)


train_len <- length(train_x)
mean_train_len <- mean(map_int(train_x, length))
print(train_len)
print(sprintf("Average train sequence length: %f", mean_train_len))

# Test sequences
tuning_len <- length(tuning_x)
mean_tuning_len <- mean(map_int(tuning_x, length))
print(tuning_len)
print(sprintf("Average tuning sequence length: %f", mean_tuning_len))


train_x  <- pad_sequences(train_x,  maxlen = maxlen, padding = "post", truncating = "post")
tuning_x <- pad_sequences(tuning_x, maxlen = maxlen, padding = "post", truncating = "post")

train_y <- to_categorical(train_y - 1L)
tuning_y <- to_categorical(tuning_y - 1L)
n_class <- ncol(train_y)

model <- keras_model_sequential()

model %>%
    layer_embedding(
        input_dim = max_features, output_dim = embedding_dims,
        input_length = maxlen
    ) %>%
    layer_global_average_pooling_1d() %>%
    layer_dense(6, activation = "sigmoid")

loss <- "categorical_crossentropy"
metrics <- "categorical_accuracy"
optimizer <- "adam"
model %>% compile(
    loss = loss,
    optimizer = optimizer,
    metrics = metrics
)


# Fitting -----------------------------------------------------------------
start_time <- now()
history <- model %>% fit(train_x, train_y,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(tuning_x, tuning_y)
)
stop_time <- now() - start_time

notes_db <- as.data.frame(history) %>%
    filter(epoch == 15) %>%
    mutate_if(is.numeric, round, 4)

plot(history) +
    xlab("Epoch") +
    ggtitle(
        "Loss and categorical accuracy by epochs for the Otiti's training and validation set.",
        subtitle = glue(
            "First exploration (only with the 'current training set', 4926 obs.)\n",
            "Training set: {length(train_indeces)} (random seed: {random_seed})\n",
            "Average training sequence length: {round(mean_train_len, 2)}\n",
            "Validation set: {length(tuning_indeces)}\n",
            "Average tuning sequence length: {round(mean_tuning_len, 2)}\n",
            "Excluded records: {length(mixdb_otiti_ngram$x) - length(admitted_indeces)} (missing entries only)\n",
            "Full dictionary size: {max(get_dictionary(mixdb_otiti_ngram))}\n",
            "Embedding (input, output): ({max_features} (padded/truncated at/from the end), {embedding_dims})\n",
            "Embedding-output layer size: {maxlen}\n",
            "Batch size: {batch_size}\n",
            "Epochs: {epochs}\n",
            "Mutually excluding classes: {n_class}\n",
            "Loss: {str_replace(loss, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Optimizer: {str_replace(optimizer, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Metric: {str_replace(metrics, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Overall time ellapsed (fit-only): {round(stop_time, 2)} {attr(stop_time, 'units')}"
        )
    ) +
    geom_text(
        data = notes_db,
        aes(x = epoch + 0.5, y = value + 0.04, label = 100 * value)
    )

current <- gsub('\\D', '', now())
gg_file <- here::here(glue("outputs/keras-gg-{current}.png"))

ggsave(gg_file, width = 8.3, height = 11.7)

model_raw <- serialize_model(model)
raw_file <- here::here(glue("outputs/keras-ws-{current}.rda"))
save(history, model_raw, file = raw_file)



model_file <- here::here(glue("outputs/keras-model-{current}.hdf5"))
save_model_hdf5(model, model_file)


