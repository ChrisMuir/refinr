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
#'   input \code{vect} a vector of business names it's recommended to set this
#'   to TRUE. Default value is TRUE.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
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

get_fingerprint_ngram <- function(vect, numgram = 2, bus_suffix = TRUE,
                                  ignore_strings = NULL) {
  numgram <- numgram - 1

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
      {gsub(regex, "", .)}
  } else {
    # Initial transformations given "ignore_strings" is NULL.
    vect <- vect %>%
      tolower %>%
      {gsub("[[:punct:]]|\\s", "", .)}
  }

  vect <- vect %>%
    strsplit(., "", fixed = TRUE) %>%
    lapply(., function(strings) {
      vapply(seq_len(length(strings) - numgram), function(char) {
        paste(strings[char:(char + numgram)], collapse = "")
      }, character(1))
    }) %>%
    cpp_list_unique(sort_vals = TRUE) %>%
    cpp_paste_collapse_list %>%
    iconv(., to = "ASCII//TRANSLIT")
  return(vect)
}
