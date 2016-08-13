#'@importFrom httr GET user_agent stop_for_status content
query <- function(key, params, ...){
  url <- paste0("http://api.wordnik.com:80/v4/", params, "&api_key=", key)
  result <- httr::GET(url, httr::user_agent("Birdnik - the wordnik client for R - https://github.com/Ironholds/birdnik"))
  httr::stop_for_status(result)
  return(httr::content(result))
}

clean_bools <- function(bool){
  if(bool == TRUE){
    return("true")
  } else if(bool == FALSE){
    return("false")
  }
  stop(sprintf("'%s' is not a valid value", bool))
}

compress_vector <- function(vector){
  return(paste(vector, collapse = ","))
}

clean_dicts <- function(dicts){
  if(length(dicts) == 1 && dicts %in% c("all", "")){
    return(dicts)
  }
  if(all(dicts %in% c("ahd", "century", "wiktionary", "wordnet", "webster"))){
    return(compress_vector(dicts))
  }
  stop("Invalid dictionaries provided")
}

clean_types <- function(types){
  if(length(types) == 1 && types == "all"){
    return("")
  }
  if(all(types %in% c("ahd", "IPA", "arpabet", "gcide-diacritical"))){
    return(compress_vector(types))
  }
  stop("Invalid pronunciation types provided")
}
