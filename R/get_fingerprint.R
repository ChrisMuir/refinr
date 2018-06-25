#' Given a character vector as input, get the key collision fingerprint for
#' each element.
#' @noRd
get_fingerprint_KC <- function(vect, bus_suffix = TRUE,
                               ignore_strings = NULL) {
  # Remove char accent marks.
  vect <- remove_accents(vect)
  if(!is.null(ignore_strings)) ignore_strings <- remove_accents(ignore_strings)
  # Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  vect <- gsub("[;'`\"]", "", tolower(vect), perl = TRUE)
  # Replace other punct with a blank space (want "cats,inc" to be 2 words).
  vect <- gsub("[[:punct:]]", " ", vect, perl = TRUE)
  vect <- gsub(" {2,}", " ", vect, perl = TRUE)
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
  vect <- strsplit(cpp_trimws_left(vect), " ", fixed = TRUE)
  # If "ignore_strings" is not NULL, for each element of list "vect", remove
  # any string that has a match within vector "ignore_strings".
  if (!is.null(ignore_strings)) vect <- remove_strings(vect, ignore_strings)
  # Final transformations, then return object "out".
  vect <- cpp_list_unique(vect, sort_vals = TRUE)
  vect <- cpp_paste_list(vect, collapse_str = " ")
  return(vect)
}

#' Given a character vector as input, get the ngram fingerprint value for each
#' element of the input.
#'@noRd
get_fingerprint_ngram <- function(vect, numgram = 2, bus_suffix = TRUE,
                                  ignore_strings = NULL) {
  # Remove char accent marks.
  vect <- remove_accents(vect)
  if(!is.null(ignore_strings)) ignore_strings <- remove_accents(ignore_strings)
  # Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  vect <- gsub("[;'`\"]", "", cpp_tolower(vect), perl = TRUE)
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
                    ")\\b| ")
    vect <- gsub(regex, "", vect, perl = TRUE)
  } else {
    # Otherwise, if ignore_strings is NULL, only remove spaces.
    vect <- gsub(" ", "", vect, fixed = TRUE)

  }
  # Rest of the transformations. For each value in vect: get ngrams, filter by
  # unique, sort alphabetically, paste back together, and normalize encoding.
  if (numgram == 1) {
    vect <- strsplit(vect, "", fixed = TRUE)
    vect <- cpp_list_unique(vect, sort_vals = TRUE)
    vect <- cpp_paste_list(vect, collapse_str = "")
  } else {
    vect <- cpp_get_char_ngrams(vect, numgram = numgram)
  }
  return(vect)
}

# Function that attempts to merge common business name suffixes within a
# character string.
business_suffix <- function(vect) {
  vect <- gsub(" incorporated| incorporate", " inc", vect, perl = TRUE)
  vect <- gsub(" corporation| corporations", " corp", vect, perl = TRUE)
  vect <- gsub(" company| companys| companies", " co", vect, perl = TRUE)
  vect <- gsub(" limited liability co", " llc", vect, fixed = TRUE)
  vect <- gsub(" limited$", " ltd", vect, perl = TRUE)
  vect <- gsub(" division| divisions", " div", vect, perl = TRUE)
  vect <- gsub(" enterprises| enterprise", " ent", vect, perl = TRUE)
  vect <- gsub(" limited partnership", " lp", vect, fixed = TRUE)
  return(vect)
}

# Remove accents from chars, while properly handling UTF-8 strings.
remove_accents <- function(vect) {
  enc <- Encoding(vect) == "UTF-8"
  vect[enc] <- stri_trans_general(vect[enc], "latin-ASCII")
  vect[!enc] <- iconv(vect[!enc], to = "ASCII//TRANSLIT")
  vect
}
