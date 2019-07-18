test_db <- tibble::tribble(
    ~class, ~id, ~notes,      ~gender,
    0  ,   1, "foo notes",     "male",
    0  ,   2, "bar notes",     "female",
    1  ,   1, "another notes", "male",
    1  ,   3, "annotated foo", "female"
)

test_mixdb <- mixdb(test_db, meta_vars(id, gender))
test_meta <- attr(test_mixdb, "meta")
test_mixdb_dictionary <- attr(test_mixdb, "dictionary")
