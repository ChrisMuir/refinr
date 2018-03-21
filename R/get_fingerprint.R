#' Given a character vector as input, get the key collision fingerprint for
#' each element.
#' @noRd
get_fingerprint_KC <- function(vect, bus_suffix = TRUE,
                               ignore_strings = NULL) {
  # Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  vect <- gsub("[;'`\"]", "", tolower(vect), perl = TRUE)
  # Replace other punct with a blank space (want "cats,inc" to be 2 words).
  vect <- gsub("[[:punct:]]", " ", vect, perl = TRUE)
  if (bus_suffix) {
    vect <- business_suffix(vect)
    if (!is.null(ignore_strings)) {
      ignore_strings <- c(ignore_strings,
                          c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                            "lp", "and"))
    } else {
      ignore_strings <- c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                          "lp", "and")
    }
  }
  vect <- strsplit(cpp_trimws_left(vect), "\\s+", perl = TRUE)
  # If "ignore_strings" is not NULL, for each element of list "vect", remove
  # any string that has a match within vector "ignore_strings".
  if (!is.null(ignore_strings)) vect <- remove_strings(vect, ignore_strings)
  # Final transformations, then return object "out".
  vect <- cpp_list_unique(vect, sort_vals = TRUE)
  vect <- cpp_paste_list(vect, collapse_str = " ")
  vect <- iconv(vect, to = "ASCII//TRANSLIT")
  vect[!nzchar(vect)] <- NA_character_
  return(vect)
}

#' Given a character vector as input, get the ngram fingerprint value for each
#' element of the input.
#'@noRd
get_fingerprint_ngram <- function(vect, numgram = 2, bus_suffix = TRUE,
                                  ignore_strings = NULL) {
  # Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  vect <- gsub("[;'`\"]", "", tolower(vect), perl = TRUE)
  # Replace other punct with a blank space (want "cats,inc" to be 2 words).
  vect <- gsub("[[:punct:]]", " ", vect, perl = TRUE)
  # Compile variable ignore_strings.
  if (bus_suffix) {
    vect <- business_suffix(vect)
    if (!is.null(ignore_strings)) {
      ignore_strings <- c(ignore_strings,
                          c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                            "lp", "and"))
    } else {
      ignore_strings <- c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                          "lp", "and")
    }
  }
  if (!is.null(ignore_strings)) {
    # Use values in "ignore_strings" to create a regex of substrings to
    # eliminate from each element of "vect" (also remove all spaces).
    regex <- paste0("\\b(",
                    paste(ignore_strings, collapse = "|"),
                    ")\\b|\\s")
  } else {
    # Otherwise, if ignore_strings is NULL, only remove spaces.
    regex <- "\\s+"
  }
  vect <- gsub(regex, "", vect, perl = TRUE)
  # Rest of the transformations. For each value in vect: get ngrams, filter by
  # unique, sort alphabetically, paste back together, and normalize encoding.
  vect <- strsplit(vect, "", fixed = TRUE)
  vect <- cpp_get_char_ngrams(vect, numgram = numgram)
  vect <- iconv(vect, to = "ASCII//TRANSLIT")
  return(vect)
}
