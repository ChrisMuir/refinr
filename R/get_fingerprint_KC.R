#' Given a character vector as input, get the key collision fingerprint for
#' each element.
#' @noRd
get_fingerprint_KC <- function(vect, bus_suffix = TRUE,
                               ignore_strings = NULL) {
  if (bus_suffix) {
    vect <- business_suffix(tolower(vect))
    if (!is.null(ignore_strings)) {
      ignore_strings <- c(ignore_strings,
                          c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                            "lp"))
    } else {
      ignore_strings <- c("inc", "corp", "co", "llc", "ltd", "div", "ent",
                          "lp")
    }
  } else {
    vect <- tolower(vect)
  }
  # Perform initial transformations.
  vect <- vect %>%
    gsub("[[:punct:]]", "", ., perl = TRUE) %>%
    cpp_trimws_left %>%
    strsplit(., "\\s+", perl = TRUE)
  # If "ignore_strings" is not NULL, for each element of list "vect", remove
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
