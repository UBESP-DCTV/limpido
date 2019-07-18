# limpido (development version)

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
