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
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint_KC("Tom's Sports Equipment, Inc.")
#' [1] "equipment sports toms"
#' }
get_fingerprint_KC <- function(vect, bus_suffix = TRUE) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))

  if (bus_suffix) {
    out <- vect %>%
      tolower %>%
      business_suffix %>%
      {gsub("[[:punct:]]", "", .)} %>%
      cpp_trimws %>%
      {gsub("\\s+", " ", .)} %>%
      strsplit(., " ", fixed = TRUE) %>%
      remove_strings(
        c("inc", "corp", "co", "llc", "ltd", "div", "ent", "lp")) %>%
      cpp_list_unique(sort_vals = TRUE) %>%
      vapply(., paste, character(1), collapse = " ") %>%
      iconv(., to = "ASCII//TRANSLIT")
    out[out == ""] <- NA_character_
  } else {
    out <- vect %>%
      tolower %>%
      {gsub("[[:punct:]]", "", .)} %>%
      cpp_trimws %>%
      {gsub("\\s+", " ", .)} %>%
      strsplit(., " ", fixed = TRUE) %>%
      cpp_list_unique(sort_vals = TRUE) %>%
      vapply(., paste, character(1), collapse = " ") %>%
      iconv(., to = "ASCII//TRANSLIT")
    out[out == ""] <- NA_character_
  }
  return(out)
}
