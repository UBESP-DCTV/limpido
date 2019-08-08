library(here)
library(limpido)

expanded <- here('../../data/pedia_gold_otiti.rds') %>%
    readRDS() %>%
    prepare_db()

expanded[(sample(seq_along(expanded), 10))]

expanded %>%
    writeLines(here("../../data/pedia_NUM_expanded.txt"))


# From command line -----------------------------------------------

# # Create pretrained models
# cd fastText-0.9.1
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_100
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_300 -dim 300


# # Query pretrained
# ./fasttext print-word-vectors model.bin < queries.txt > res_word.txt
# ./fasttext print-sentence-vectors model.bin < queries.txt > res_sentence.txt




