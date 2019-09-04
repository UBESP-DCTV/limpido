library(here)
library(limpido)
library(depigner)
start_bot_for_chat("otiti")
errors_to_telegram("otiti")


expanded <- here('../data/pedia_gold_otiti.rds') %>%
    readRDS()
expanded_txt <- prepare_db(expanded)

send_to_telegram("expanded created")

expanded_txt[(sample(seq_along(expanded_txt), 10))]

expanded_txt %>%
    writeLines(here("../data/pedia_NUM_expanded.txt"))

send_to_telegram("expanded written on disk!")

# From command line -----------------------------------------------

# # Create pretrained models
# cd fastText-0.9.1
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_100
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_300 -dim 300


# # Query pretrained
# ./fasttext print-word-vectors model.bin < queries.txt > res_word.txt
# ./fasttext print-sentence-vectors model.bin < queries.txt > res_sentence.txt




