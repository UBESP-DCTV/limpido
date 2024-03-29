* Fixed records classes (rows of `attr(mixdb_otiti_tagged, "meta")`):
  - c(4999, 5574) to "1",
  - c(5140, 4929, 5039, 5282, 5039, 4978) to "2",
  - c(5631) to three
* Added function to randomly guess classes as prediction baseline
* Added first exploration architecture for analyses
* Added function to get statistics on predictions
* Added info to plots and saved objects

# limpido 0.1.1

* Add function `save_all_models()` to store al the relevant information
  and plot from the trained model.
* Add function `setup_input_data()` to initial setup for the training 

# limpido 0.1.0

* Add script in `data-raw` to create the pretrained embeddings from 
  the full pedianet db.
* Add `prepare_db()` to convert the final pedianet dataframe to a
  character vector usefull for FastText
* incorporate explicit `__NA__` tag for missing entries in the creation
  of a `mixdb`
* Change tags convention from `[<tag>]` to `__<tag>__`

# limpido 0.0.0.9002

* Add `code_num` to coding numbers in a corpus
* Add `expand_punctuations()` to separate words from other characters
* Added `create_ngram()` and `add_ngram()` to add engram to a `mixdb`.
  `create_ngram()` can be usefull stand-alone too.
* Added constructor for the class `mixdb`. `mixdb` takes in input a 
  dataframe with some column of text, and permit to select metadata to
  store and exclude from the textual parsing. Next, it store the text
  as named integer, including attributs for the dictionary and the
  frequency of the token.
* Update folder (removing data, including analyses)
* Update codecov and Travis CI setup

# limpido 0.0.0.9001

* Added licence, CoC, basic badges
* Added basic support for package development, CI, tests, git/GitHub,
  spellcheck.

# limpido 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
