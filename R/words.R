#'@title Retrieve definitions of a word
#'@description \code{word_definitions} retrieves the overall metadata for
#'each word in \code{words}, including actual definitions.
#'
#'@param key a Wordnik API key. These can be obtained
#'at the \href{http://developer.wordnik.com/}{Wordnik developer portal}.
#'
#'@param words a vector of words.
#'
#'@param use_canonical whether to use the stemmed, canonical form of the word
#'(i.e. 'cat', for 'cats') instead of the actual \code{word}. FALSE by default.
#'Note that for pronunciations specifically, setting it to TRUE may get a wider
#'range of pronunciations; you should experiment.
#'
#'@param source_dicts the dictionaries to use; options are any permutation of
#'"ahd", "century", "wiktionary", "wordnet" and "webster". "all" (the default)
#'searches all of them.
#'
#'@param limit the maximum number of items to return for any one word.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@return a list structure containing definitions for each word in \code{words},
#'along with associated metadata, which can be manipulated with
#'\code{\link{birdnik_get}}.
#'
#'@seealso \code{\link{birdnik_get}} for ways to conveniently access individual
#'chunks of the returned dataset.
#'
#'@examples
#'\dontrun{
#'cats <- word_definitions(key = "madeupkey", words = "cat")
#'}
#'@export
word_definitions <- function(key, words, use_canonical = FALSE,
                             source_dicts = "all",
                             limit = 200, ...){

  stopifnot(length(use_canonical) == 1)

  param <- paste0("word.json/", words, "/definitions?limit=", limit,
                  "&includeRelated=false",
                  "&includeTags=false",
                  "&sourceDictionaries=", clean_dicts(source_dicts),
                  "&useCanonical=", clean_bools(use_canonical))

  return(lapply(param, query, key = key))
}

#'@title Retrieve a Word's Top Example
#'
#'@description \code{word_top_example} grabs, as it suggests, the top example for
#'each entry in \code{words}.
#'
#'@inheritParams word_definitions
#'
#'@return a data.frame of 5 columns; \code{provider}, \code{year},
#'\code{rating}, \code{word} and \code{example}.
#'
#'@examples
#'\dontrun{
#'examples <- word_top_example(key = "madeupkey", words = c("cat", "dog", "turnip"))
#'}
#'@export
word_top_example <- function(key, words, use_canonical = FALSE, ...){

  stopifnot(length(use_canonical) == 1)

  param <- paste0("word.json/", words,
                  "/topExample?useCanonical=", clean_bools(use_canonical))

  return(do.call("rbind", mapply(function(param, words, key, ...){
    result <- try({
      result <- query(key, param, ...)
    }, silent = TRUE)
    if("try-error" %in% class(result)){
      return(data.frame(provider = NA, year = NA, rating = NA,
                        word = words, example = NA, stringsAsFactors = FALSE))
    }
    return(data.frame(provider = result$provider$name,
                      year = result$year,
                      rating = result$rating,
                      word = result$word,
                      example = result$text,
                      stringsAsFactors = FALSE))
  }, param = param, words = words, key = key, ...,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)))
}

#'@title Retrieve related words
#'
#'@description \code{related_words} grabs terms that are in some way associated
#'with the \code{words} argument.
#'
#'@inheritParams word_definitions
#'
#'@return a data.frame of 3 columns: \code{word} (the original term),
#'\code{type} (the type of relationship) and \code{related_word}.
#'
#'@examples
#'\dontrun{
#'cat_adjacent <- related_words(key = "madeupkey", words = "cat")
#'}
#'@export
related_words <- function(key, words, use_canonical = FALSE, ...){
  stopifnot(length(use_canonical) == 1)

  param <- paste0("word.json/", words,
                  "/relatedWords?useCanonical=", clean_bools(use_canonical))

  return(do.call("rbind", mapply(function(param, key, word, ...){
    result <- query(key, param, ...)

    if(!length(result)){
      return(data.frame(word = word, type = NA, related_word = NA,
                        stringsAsFactors = FALSE))
    }
    return(do.call("rbind", lapply(result, function(entry, word){
      return(data.frame(word = word,
                        type = unname(unlist(entry$relationshipType)),
                        related_word = unname(unlist(entry$words)),
                        stringsAsFactors = FALSE))
    }, word = word)))
  }, param = param, key = key, word = words, ..., SIMPLIFY = FALSE,
  USE.NAMES = FALSE)))
}

#'@title Retrieve Pronunciations for Words
#'@description \code{word_pronunciations} grabs, well,
#'pronunciations for a series of words.
#'
#'@param pronunciation_type the type of pronunciations to return, if available;
#'options are "ahd", "IPA", "arpabet" and "gcide-diacritical". "all" (the
#'default) provides any that are available.
#'
#'@inheritParams word_definitions
#'@examples
#'\dontrun{
#'cats_vs_dogs <- word_pronunciations(key = "fakekey", words = c("cat", "dog"))
#'}
#'@export
word_pronunciations <- function(key, words, use_canonical = FALSE,
                                 source_dicts = "all",
                                 pronunciation_type = "all", limit = 50,
                                 ...){
  stopifnot(length(use_canonical) == 1)
  if(source_dicts[1] == "all"){
    source_dicts <- ""
  }

  sources <- clean_dicts(source_dicts)
  types <- clean_types(pronunciation_type)
  param <- paste0("word.json/", words,
                  "/pronunciations?useCanonical=", clean_bools(use_canonical),
                  ifelse(sources == "", "", paste0("&sourceDictionary=",
                                                   sources)),
                  ifelse(types == "", "", paste0("&typeFormat=", types)),
                  "&limit=", limit)

  return(do.call("rbind", mapply(FUN = function(param, key, word, ...){
    result <- query(key, param, ...)
    if(!length(result)){
      return(data.frame(word = word, type = NA, type_seq = NA,
                        pronunciation = NA, stringsAsFactors = FALSE))
    }
    result <- unlist(result)
    return(data.frame(word = word, type = unname(result[names(result) == "rawType"]),
                      type_seq = as.integer(unname(result[names(result) == "seq"])),
                      pronunciation = unname(result[names(result) == "raw"]),
                      stringsAsFactors = FALSE))
  }, param = param, key = key, word = words, ..., SIMPLIFY = FALSE,
  USE.NAMES = FALSE)))
}

#'@title Count the per-year frequency of words
#'@description \code{word_frequency} provides, for a vector of words,
#'the number of appearances each word made per year in the source texts Wordnik
#'uses.
#'
#'@param start_year the earliest year to get frequencies for. 1800 (the earliest
#'accepted value) by default.
#'
#'@param end_year the latest year to get frequencies for. 2012 (the latest accepted
#'value) by default.
#'
#'@inheritParams word_definitions
#'
#'@return a data.frame of 3 columns; \code{word}, \code{year} and \code{frequency}.
#'
#'@examples
#'\dontrun{
#'cats_versus_dogs <- word_frequency(key = "notarealkey", words = c("cat", "dog"))
#'}
#'
#'@export
word_frequency <- function(key, words, use_canonical = FALSE, start_year = 1800,
                           end_year = 2012, ...){

  stopifnot(length(use_canonical) == 1)
  param <- paste0("word.json/", words,
                  "/frequency?useCanonical=", clean_bools(use_canonical),
                  "&startYear=", start_year,
                  "&endYear=", end_year)

  return(do.call("rbind", lapply(param, function(x, key, ...){
    single_result <- query(key, x, ...)
    frequencies <- unlist(single_result$frequency)
    return(data.frame(word = single_result$word,
                      year = frequencies[names(frequencies) == "year"],
                      count = frequencies[names(frequencies) == "count"],
                      stringsAsFactors = FALSE))
  }, key = key, ...)))
}

#'@title Retrieve word bigrams
#'@description \code{word_bigrams} grabs bigrams for whatever words you want
#'out of the wordnik database, along with the match strength (weighted
#'with and without factoring in word length).
#'
#'@param min_wlmi the minimum (word-length weighted) strength of the similarity
#'between the bigram words.
#'
#'@inheritParams word_definitions
#'
#'@return a data.frame of 5 columns; \code{word}, \code{first_gram},
#'\code{second_gram}, \code{mi} (the strength of the relationship) and
#'\code{wlmi} (the strength of the relationship, weighted by word length).
#'
#'@examples
#'\dontrun{
#'
#'dog_associations <- word_bigrams(key = "madeupkey", words = "dog")
#'#mauling and fighting. But also biscuits!
#'}
#'@export
word_bigrams <- function(key, words, use_canonical = FALSE,
                         min_wlmi = 0, limit = 5, ...){

  param <- paste0("word.json/", words,
                  "/phrases?useCanonical=", clean_bools(use_canonical),
                  "&wlmi=", min_wlmi,
                  "&limit=", limit)

  return(do.call("rbind", mapply(function(param, word, key, ...){
    result <- query(key, param, ...)
    if(!length(result)){
      return(data.frame(word = word, first_gram = NA, second_gram = NA,
                        mi = NA, wlmi = NA, stringsAsFactors = FALSE))
    }
    result <- unlist(result)
    return(data.frame(word = word,
                      first_gram = unname(result[names(result) == "gram1"]),
                      second_gram = unname(result[names(result) == "gram2"]),
                      mi = as.numeric(unname(result[names(result) == "mi"])),
                      wlmi = as.numeric(unname(result[names(result) == "wlmi"])),
                      stringsAsFactors = FALSE))
  }, param = param, word = words, key = key, ...,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)))
}
