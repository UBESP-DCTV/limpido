# Attach packages =================================================

library(limpido)
library(tidyverse)
library(lubridate)                          # to attach before here
library(keras)
library(here)
library(glue)
library(usethis)


# Constants =======================================================

data_path <- here("../../data/")
output_path <- here("../../output/")

random_seed <- sample.int(1e4, 1)
set.seed(random_seed)

# n_train <- 4600              # cases in the train + train_val set
validation_len <- 300L                # cases in the validation set

embedding_dim <- 300L                 # embedding vectors dimension

max_words     <- min(                        # max words considered
    # XXXL  # max number of words the user admit
    10733L, # all 1gram in the train-validation
    122607  # all 1-gram in the pretrained
)
maxlen <- 300L                    # max words/token per observation

batch_size <- 8L                      # sample to pass in each feed
epochs <- 15L               # whole passage throug the training set

# Loading data ====================================================


# Training and validation -----------------------------------------

cache_1gram <- file.path(data_path, "mixdb_otiti_tagged_1gram.rda")

if (file.exists(cache_1gram)) {
    load(cache_1gram)
} else {
    gold_otiti <- here::here("../../data/pedia_gold_otiti.rds") %>%
        readRDS() %>%
        filter(!is.na(class))

    mixdb_otiti_tagged <- gold_otiti %>%
        mutate_if(is.character, stringr::str_to_lower) %>%
        # HERE WE SEPARATE PUNCTUATIONS AND ADD TAGS FOR NUMBERS
        mutate_if(is.character, code_num) %>%
        mutate_if(is.character, expand_punctuations) %>%
        mutate_if(is.character, replace_na, "__NA__") %>%
        mixdb(meta_vars(
            set, id_medico, guidpaziente, datacontatto, oracontatto,
            data_nascita, sesso
        ))
    save(mixdb_otiti_tagged, file = cache_1gram)
}

mixdb_otiti_tagged %>% str(1)
token_per_case <- mixdb_otiti_tagged[["x"]] %>%
    map_int(length)

ui_info("min token/observation: {ui_value(min(token_per_case))}")
ui_info("IQ token/observation: {ui_value(quantile(token_per_case, 0.25))}")
ui_info("mean token/observation: {ui_value(mean(token_per_case))}")
ui_info("median token/observation: {ui_value(median(token_per_case))}")
ui_info("IIIQ token/observation: {ui_value(quantile(token_per_case, 0.75))}")
ui_info("p95 token/observation: {ui_value(quantile(token_per_case, 0.95))}")
ui_info("p99 token/observation: {ui_value(quantile(token_per_case, 0.99))}")
ui_info("max token/observation: {ui_value(max(token_per_case))}")


# number of training and validation samples
sets <- attr(mixdb_otiti_tagged, "meta")$set
sets_len <- table(sets)

# remove some indeces if it is problematic
admitted_cases_indeces  <- seq_len(sum(sets_len))

all_validation_indeces <- which(sets == "validation")
validation_indeces <- sample(all_validation_indeces, validation_len)

# In the training set we include a portion of the validation to
# incorporate information from teh distribution of the validation/test
# set.
train_indeces <- admitted_cases_indeces[-validation_indeces]

train_x <- mixdb_otiti_tagged$x[train_indeces] %>%
  map(~{
    is_oob <- .x > max_words
    .x[is_oob] <- max_words + 1L
    names(.x[is_oob]) <- rep("__OOB__", sum(is_oob))
    .x
})

train_y <- as.integer(mixdb_otiti_tagged$y[train_indeces])

validation_x <- mixdb_otiti_tagged$x[validation_indeces] %>%
    map(~{
        is_oob <- .x > max_words
        .x[is_oob] <- max_words + 1L
        names(.x[is_oob]) <- rep("__OOB__", sum(is_oob))
        .x
    })

validation_y <- as.integer(mixdb_otiti_tagged$y[validation_indeces])


names(train_y) <- rep("train", length(train_y))
names(validation_y) <- rep("validation", length(validation_y))

tibble(
    class = c(train_y, validation_y),
    set = names(c(train_y, validation_y))
) %>%
    group_by(set, class) %>%
    summarise(n = n()) %>%
    mutate(p = n/sum(n)) %>%
    ggplot(aes(x = class, y = p, fill = set)) +
    geom_bar(stat = "identity", position = position_dodge())

train_x  <- pad_sequences(train_x,
    maxlen = maxlen,
    padding = "post",
    truncating = "post"
)
validation_x <- pad_sequences(validation_x,
    maxlen = maxlen,
    padding = "post",
    truncating = "post"
)

train_y      <- to_categorical(train_y - 1L)
validation_y <- to_categorical(validation_y - 1L)
n_class <- ncol(train_y)


# Pretrained model ------------------------------------------------

model_100 <- file.path(data_path, "model_100.vec") %>%
    read_lines(skip = 1L)
model_300 <- file.path(data_path, "model_300.vec") %>%
    read_lines(skip = 1L)



values <- stringr::str_split(model_300, " ")
word   <- character(1L)
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())

for (i in seq_along(model_300)) {

  word <- values[[i]][[1L]]
  vctr <- as.double(values[[i]][-c(1L, 302L)])
  stopifnot(length(vctr) == 300L)

  embeddings_index[[word]] <- vctr

 }

# Embedding matrix ------------------------------------------------

words <- c(
  names(get_dictionary(mixdb_otiti_tagged)[seq_len(max_words)]),
  "__OOB__"
)
max_words <- max_words + 1L
embedding_matrix <- array(0, c(length(words), embedding_dim))
embedding_vector <- double(embedding_dim)

for (i in seq_along(words)) {

    word <- words[[i]]
    embedding_vector <- embeddings_index[[word]]
    # ui_info(pryr::address(embedding_vector))
    # Words not found in the embedding index will be all zeros.
    if (is.null(embedding_vector)) next

    embedding_matrix[i, ] <- embedding_vector

}

## by default, fasttext do not include words with frequancy less
## than 5

# words[which(rowSums(embedding_matrix) == 0)]


# Model ===========================================================

loss <- "categorical_crossentropy"
metrics <- "categorical_accuracy"
optimizer <- "adam"

model <- keras_model_sequential() %>%
    layer_embedding(
        input_length = maxlen,                      # neurons in layer 0
        input_dim = max_words,  # used dictionary size (embeddings rows)
        output_dim = embedding_dim,           # vector embeddings length
        trainable = TRUE,
        weights = list(embedding_matrix)
    ) %>%
    # layer_global_max_pooling_1d() %>%
    layer_flatten() %>% # layer_global_average_pooling_1d() %>% #
    layer_dense(units = 32L, activation = "relu") %>%
    layer_dense(units = 6L, activation = "sigmoid")

summary(model)

model %>% compile(
    loss = loss,
    optimizer = optimizer,
    metrics = metrics
)


{
  start_time <- now()
  history <- model %>%
      fit(train_x, train_y,
          batch_size = batch_size,
          epochs = epochs,
          validation_data = list(validation_x, validation_y)
  )
  stop_time <- now() - start_time
}

current <- gsub('\\D', '', now())
model_file <- file.path(output_path, glue("keras-model-{current}.hdf5"))
save_model_hdf5(model, model_file)

model_raw <- serialize_model(model)
raw_file <- file.path(output_path, glue("keras-ws-{current}.rda"))
save(history, model_raw, file = raw_file)


# Plots and summaries ---------------------------------------------

notes_db <- as.data.frame(history) %>%
    filter(epoch == 15) %>%
    mutate_if(is.numeric, round, 4)

mean_train_len <- mean(map_int(train_x, length))
mean_validation_len <- mean(map_int(validation_x, length))

plot(history) +
    xlab("Epoch") +
    ggtitle(
        "Loss and categorical accuracy by epochs for the Otiti's training and validation set.",
        subtitle = glue(
            "Full exploration (5649 observation overall)\n",
            "Training set: {length(train_indeces)} (random seed: {random_seed})\n",
            "Average training sequence length: {round(mean_train_len, 2)}\n",
            "Validation set: {length(validation_indeces)}\n",
            "Average validation sequence length: {round(mean_validation_len, 2)}\n",
            "Full dictionary size: {max(get_dictionary(mixdb_otiti_tagged))}\n",
            "Embedding (input, output): ({max_words} (padded/truncated at/from the end), {embedding_dim})\n",
            "Embedding-output layer size: {maxlen}\n",
            "Batch size: {batch_size}\n",
            "Epochs: {epochs}\n",
            "Mutually excluding classes: {n_class}\n",
            "Loss: {str_replace(loss, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Optimizer: {str_replace(optimizer, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Optimization metric: {str_replace(metrics, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
            "Overall time ellapsed (fit-only): {round(stop_time, 2)} {attr(stop_time, 'units')}"
        )
    ) +
    geom_text(
        data = notes_db,
        aes(x = epoch + 0.5, y = value + 0.04, label = 100 * value)
    )

gg_file <- file.path(output_path, glue("keras-gg-{current}.png"))
ggsave(gg_file, width = 8.3, height = 11.7)




