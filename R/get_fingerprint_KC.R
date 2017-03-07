#' Get key collision fingerprints
#'
#' Given a character vector as input, get the key collision fingerprint for
#' each element. For more details on key collision, see
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   working with a vector of business names it's recommended to set this to
#'   TRUE. Default value is TRUE.
#'
#' @return Key collision value of the input string.
#' @importFrom dplyr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint_KC("Tom's Sports Equipment, Inc.")
#' [1] "equipment sports toms"
#' }
get_fingerprint_KC <- function(vect, bus_suffix) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))

  if (bus_suffix) {
    vect %>%
      tolower %>%
      business_suffix %>%
      {gsub("[[:punct:]]", "", .)} %>%
      trimws %>%
      {gsub("\\s{2,}", " ", .)} %>%
      strsplit(., " ", fixed = TRUE) %>%
      lapply(., function(x)
        x[!x %in% c("inc", "corp", "co", "llc", "ltd", "div", "ent", "lp")]) %>%
      lapply(., sort) %>%
      lapply(., unique) %>%
      vapply(., function(x) paste(x, collapse = " "), character(1)) %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      sapply(., function(x) if (x == "" || is.na(x)) {NA} else {x},
             USE.NAMES = FALSE)
  } else {
    vect %>%
      tolower %>%
      {gsub("[[:punct:]]", "", .)} %>%
      trimws %>%
      {gsub("\\s{2,}", " ", .)} %>%
      strsplit(., " ", fixed = TRUE) %>%
      lapply(., sort) %>%
      lapply(., unique) %>%
      vapply(., function(x) paste(x, collapse = " "), character(1)) %>%
      iconv(., to = "ASCII//TRANSLIT") %>%
      sapply(., function(x) if (x == "" || is.na(x)) {NA} else {x},
             USE.NAMES = FALSE)
  }
}
