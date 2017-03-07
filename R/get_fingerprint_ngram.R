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
#' @importFrom dplyr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint_ngram("Tom's Sports Equipment, Inc.", numgram = 1)
#' [1] "eimnopqrstu"
#' get_fingerprint_ngram("Tom's Sports Equipment, Inc.", numgram = 2)
#' [1] "eneqipmemsntomorpmpoqurtsespsstotsui"
#' }
get_fingerprint_ngram <- function(vect, numgram, bus_suffix) {
  ## TODO: ----
  ## Add utf encoding normalization to both the if leg and else leg of this
  ## function (to eliminate accent marks)
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))

  if (bus_suffix) {
    vect %>%
      tolower %>%
      business_suffix %>%
      {gsub("\\b(inc|corp|co|llc|ltd|div|ent|lp)\\b", "", .)} %>%
      {gsub("[[:punct:]]|\\s", "", .)} %>%

      sapply(., function(x) {
        if (!is.na(x)) {
          ngram::splitter(x, split.char = TRUE, split.space = FALSE)
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      sapply(., function(x) {
        if (!is.na(x) && nchar(x) >= (numgram + (numgram - 1))) {
          x
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      sapply(., function(x) {
        if (!is.na(x)) {
          ngram::ngram(x, n = numgram)
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      lapply(., function(x) {
        if (class(x) == "ngram") {
          ngram::get.ngrams(x)
        } else {
          NA
        }}) %>%

      lapply(., sort) %>%
      lapply(., unique) %>%
      vapply(., function(x) paste(x, collapse = ""), character(1)) %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      {gsub("\\s", "", .)} %>%
      ifelse(. == "" || is.na(.), NA, .)
  } else {
    vect %>%
      tolower %>%
      {gsub("[[:punct:]]|\\s", "", .)} %>%

      sapply(., function(x) {
        if (!is.na(x)) {
          ngram::splitter(x, split.char = TRUE, split.space = FALSE)
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      sapply(., function(x) {
        if (!is.na(x) && nchar(x) >= (numgram + (numgram - 1))) {
          x
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      sapply(., function(x) {
        if (!is.na(x)) {
          ngram::ngram(x, n = numgram)
        } else {
          NA
        }}, USE.NAMES = FALSE) %>%

      lapply(., function(x) {
        if (class(x) == "ngram") {
          ngram::get.ngrams(x)
        } else {
          NA
        }}) %>%

      lapply(., sort) %>%
      lapply(., unique) %>%
      vapply(., function(x) paste(x, collapse = ""), character(1)) %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      {gsub("\\s", "", .)} %>%
      ifelse(. == "" || is.na(.), NA, .)
  }
}
