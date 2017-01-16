get_core <- function(results, component, name){
  output <- do.call("rbind", lapply(results, function(result, comp){
    holding <- unlist(result)
    if(!length(holding)){
      return(NULL)
    }

    return(data.frame(word = unname(holding[names(holding) == "word"][1]),
                      x = unique(unname(holding[names(holding) == comp])),
                      stringsAsFactors = FALSE))
  }, comp = component))
  names(output)[2] <- name
  return(output)
}

#'@title Extract components from word metadata
#'@description these functions allow you to extract particular components
#'from wordnik definitions; \code{get_pos} the unique parts-of-speech,
#'\code{get_text} the unique definitions for each word, and \code{get_score}
#'the unique word scores.
#'
#'@param results the results of a call to \code{\link{word_definitions}}.
#'
#'@return a data.frame of two columns - \code{word} (containing the word)
#'and a second column of \code{score}, \code{text} or \code{part_of_speech}
#'depending on the function you called.
#'
#'@examples
#'\dontrun{
#'# Retrieve the unique parts-of-speech for 'no' and 'kings'
#'no_kings <- word_definitions(key = "madeupkey", words = c("no", "kings"))
#'get_pos(no_kings)
#'}
#'
#'@export
#'@aliases birdnik_get
#'@rdname birdnik_get
get_pos <- function(results){
  return(get_core(results, "partOfSpeech", "part_of_speech"))
}

#'@export
#'@rdname birdnik_get
get_text <- function(results){
  return(get_core(results, "text", "text"))
}

#'@export
#'@rdname birdnik_get
get_score <- function(results){
  return(get_core(results, "score", "score"))
}