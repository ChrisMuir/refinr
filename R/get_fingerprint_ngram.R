#' Given a character vector as input, get the ngram fingerprint value for each
#' element of the input.
#'
#'@noRd
get_fingerprint_ngram <- function(vect, numgram = 2, bus_suffix = TRUE,
                                  ignore_strings = NULL) {
  # Compile variable ignore_strings.
  if (bus_suffix) {
    if (!is.null(ignore_strings)) {
      ignore_strings <- c(ignore_strings,
                          c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                            "lp"))
    } else {
      ignore_strings <- c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                          "lp")
    }
  }

  if (!is.null(ignore_strings)) {
    # Initial transformations given "ignore_strings" is not NULL.
    #
    # Use values in "ignore_strings" to create a regex of substrings to
    # eliminate from each element of "vect" (also remove all punctuation
    # and spaces).
    regex <- paste0("\\b(",
                    paste(ignore_strings, collapse = "|"),
                    ")\\b|[[:punct:]]|\\s")
    vect <- vect %>%
      tolower %>%
      business_suffix %>%
      {gsub(regex, "", ., perl = TRUE)}
  } else {
    # Initial transformations given "ignore_strings" is NULL.
    vect <- vect %>%
      tolower %>%
      {gsub("[[:punct:]]|\\s", "", ., perl = TRUE)}
  }

  # Rest of the transformations. For each value in vect: get ngrams, filter by
  # unique, sort alphabetically, paste back together, and normalize encoding.
  vect <- vect %>%
    strsplit("", fixed = TRUE) %>%
    cpp_get_char_ngrams(numgram = numgram) %>%
    iconv(to = "ASCII//TRANSLIT")
  return(vect)
}
