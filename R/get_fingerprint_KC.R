#' Get key collision fingerprints
#'
#' Given a character vector as input, get the key collision fingerprint for
#' each element. For more details on key collision, see
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   input \code{vect} a vector of business names it's recommended to set this
#'   to TRUE. Default value is TRUE.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
#'
#' @return Key collision value of the input string.
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#' get_fingerprint_KC("Tom's Sports Equipment, Inc.")
#' [1] "equipment sports toms"
#' }
get_fingerprint_KC <- function(vect, bus_suffix = TRUE,
                               ignore_strings = NULL) {
  if (bus_suffix) {
    # If "bus_suffix" is TRUE, make all values in "vect" lower case, then
    # perform normalization of common business suffix strings.
    vect <- business_suffix(tolower(vect))
    if (!is.null(ignore_strings)) {
      # If "ignore_strings" is not NULL, add common business suffix
      # abbreviations to vector "ignore_strings".
      ignore_strings <- c(ignore_strings,
                          c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                            "lp"))
    } else {
      # If "ignore_strings" is NULL, initialize the variable with common
      # business suffix abbreviations.
      ignore_strings <- c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                          "lp")
    }
  } else {
    # If bus_suffix is FALSE, make all values in "vect" lower case.
    vect <- tolower(vect)
  }

  # Perform initial transformations.
  vect <- vect %>%
    {gsub("[[:punct:]]", "", .)} %>%
    cpp_trimws %>%
    {gsub("\\s+", " ", .)} %>%
    strsplit(., " ", fixed = TRUE)
  # If "ignore_strings" is not NULL, for each element of list "out", remove
  # any string that has a match within vector "ignore_strings".
  if (!is.null(ignore_strings)) vect <- remove_strings(vect, ignore_strings)
  # Final transformations, then return object "out".
  vect <- vect %>%
    cpp_list_unique(sort_vals = TRUE) %>%
    vapply(., paste, character(1), collapse = " ") %>%
    iconv(., to = "ASCII//TRANSLIT")
  vect[vect == ""] <- NA_character_
  return(vect)
}
