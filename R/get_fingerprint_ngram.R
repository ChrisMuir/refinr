#' Get ngram fingerprints
#'
#' Given a character vector as input, get the ngram fingerprint value for each
#' element of the input.
#' For more details on ngram fingerprinting, see
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector.
#' @param numgram Numeric value indicating the number of characters that
#'   will occupy each token (default value is 2).
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   working with a vector of business names it's recommended to set this to
#'   TRUE. Default value is TRUE.
#'
#' @return Ngram values of the input vector.
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint_ngram("Tom's Sports Equipment, Inc.", numgram = 1)
#' [1] "eimnopqrstu"
#' get_fingerprint_ngram("Tom's Sports Equipment, Inc.", numgram = 2)
#' [1] "eneqipmemsntomorpmpoqurtsespsstotsui"
#' }

get_fingerprint_ngram <- function(vect, numgram = 2, bus_suffix = TRUE) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))

  numgram_thres <- numgram + (numgram - 1)

  if (bus_suffix) {
    # Make initial transformations to all non-NA elements of vect. Remove all
    # business suffix characters from each string.
    vect <- vect %>%
      tolower %>%
      business_suffix %>%
      {gsub("\\b(inc|corp|co|llc|ltd|div|ent|lp)\\b", "", .)} %>%
      {gsub("[[:punct:]]|\\s", "", .)} %>%
      char_splitter(numgram_thres)
  } else {
    # Make initial transformations to all non-NA elements of vect. Spare all
    # business suffix characters from each string.
    vect <- vect %>%
      tolower %>%
      {gsub("[[:punct:]]|\\s", "", .)} %>%
      char_splitter(numgram_thres)
  }

  # Get indices of vect that are not NA again (NA's could have been introduced
  # in the steps above).
  vect_non_na <- !is.na(vect)

  if (numgram > 1) {
    # If numgram > 1, use the ngram pkg to get char grams.
    vect[vect_non_na] <- vect[vect_non_na] %>%
      lapply(., function(x) {
        ngram::get.ngrams(ngram::ngram(x, n = numgram))
      }) %>%
      cpp_list_unique(sort_vals = TRUE) %>%
      vapply(., paste, character(1), collapse = "") %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      {gsub("\\s", "", .)}
  } else if (numgram == 1) {
    # Else if numgram == 1, use base R to get char unigrams.
    vect[vect_non_na] <- vect[vect_non_na] %>%
      strsplit(., " ", fixed = TRUE) %>%
      cpp_list_unique(sort_vals = TRUE) %>%
      vapply(., paste, character(1), collapse = "") %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      {gsub("\\s", "", .)}
  }
  return(vect)
}
