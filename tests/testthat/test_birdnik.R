testthat::context("Test birdnik")

# Key for tests. Please do *not* use it for other purposes.
key <- "a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5"

testthat::test_that("Definition retrieval works", {
  result <- word_definitions(key = key, words = "cat")
  testthat::expect_true(is.list(result))
  testthat::expect_equal(length(result), 1)
})

testthat::test_that("Example retrieval works", {
  result <- word_top_example(key = key, words = c("cat", "dog", "turnip", "madeupkey"))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_true(is.na(result$provider[4]))
  testthat::expect_true(all(names(result) %in% c("provider", "year", "rating", "word", "example")))
})

testthat::test_that("Example retrieval works", {
  result <- related_words(key = key, words = c("cat", "madeupkey"))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 3)
  testthat::expect_true(is.na(result$type[result$word == "madeupkey"]))
  testthat::expect_true(all(names(result) %in% c("type", "related_word", "word")))
})

testthat::test_that("pronounciation retrieval works", {
  result <- word_pronunciations(key = key, words = c("cat", "dog"))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 4)
  testthat::expect_true(all(names(result) %in% c("type", "type_seq", "word", "pronunciation")))
})


testthat::test_that("frequency retrieval works", {
  result <- word_frequency(key = key, words = c("cat", "dog"))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 3)
  testthat::expect_true(all(names(result) %in% c("word", "year", "count")))
})

testthat::test_that("bigram retrieval works", {
  result <- word_bigrams(key = key, words = c("cat", "dog"))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 5)
  testthat::expect_true(all(names(result) %in% c("word", "first_gram", "second_gram", "mi", "wlmi")))
})