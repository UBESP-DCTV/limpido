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

