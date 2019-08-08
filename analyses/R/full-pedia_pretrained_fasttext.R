library(here)
library(limpido)

data_path <- here("../data-raw/dataset_final.rda")
load(data_path)

expanded <- prepare_db(dataset_final)

expanded %>%
    `[`(sample(seq_along(.), 10))

expanded %>%
    writeLines(here("../data/pedia_NUM_expanded.txt"))

# cd fastText-0.9.1
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_100
# ./fasttext skipgram -input ~/2019-otiti/data/pedia_NUM_expanded.txt -output ~/2019-otiti/data/model_300 -dim 300
# ./fasttext print-word-vectors model.bin < queries.txt > res_word.txt
# ./fasttext print-sentence-vectors model.bin < queries.txt > res_sentence.txt




