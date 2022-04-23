#' Value merging based on Key Collision
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It clusters values
#' based on the key collision method, described here
#' \url{https://docs.openrefine.org/next/technical-reference/clustering-in-depth/}.
#'
#' @param vect Character vector, items to be potentially clustered and merged.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
#' @param bus_suffix Logical, indicating whether the merging of records should
#'   be insensitive to common business suffixes or not. Default value is TRUE.
#' @param dict Character vector, meant to act as a dictionary during the
#'   merging process. If any items within \code{vect} have a match in dict,
#'   then those items will always be edited to be identical to their match in
#'   dict. Default value is NULL.
#'
#' @return Character vector with similar values merged.
#' @export
#'
#' @examples
#' x <- c("Acme Pizza, Inc.", "ACME PIZZA COMPANY", "pizza, acme llc",
#'        "Acme Pizza, Inc.")
#' key_collision_merge(vect = x)
#'
#' # Use parameter "dict" to influence how clustered values are edited.
#' key_collision_merge(vect = x, dict = c("Nicks Pizza", "acme PIZZA inc"))
#'
#' # Use parameter 'ignore_strings' to ignore specific strings during merging
#' # of values.
#' x <- c("Bakersfield Highschool", "BAKERSFIELD high",
#'        "high school, bakersfield")
#' key_collision_merge(x, ignore_strings = c("high", "school", "highschool"))
#'
key_collision_merge <- function(vect, ignore_strings = NULL, bus_suffix = TRUE,
                                dict = NULL) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))
  stopifnot(is.null(dict) || is.character(dict))
  stopifnot(is.null(ignore_strings) || is.character(ignore_strings))

  # If dict is not NULL, remove NA's and get unique values of dict.
  is_dict_null <- is.null(dict)
  if (!is_dict_null) dict <- cpp_unique(dict[!is.na(dict)])

  # If ignore_strings is not NULL, make all values lower case then get uniques.
  if (!is.null(ignore_strings)) {
    ignore_strings <- unique(
      cpp_tolower(ignore_strings[!is.na(ignore_strings)])
    )
  }

  # Get vector of key values. If dict is not NULL, get vector of key values
  # for dict as well.
  keys_vect <- get_fingerprint_KC(vect, bus_suffix, ignore_strings)
  if (!is_dict_null) {
    keys_dict <- get_fingerprint_KC(dict, bus_suffix, ignore_strings)
  } else {
    keys_dict <- NA_character_
    dict <- NA_character_
  }

  # Make mass edits to the values of vect related to each cluster.
  return(merge_KC_clusters(vect, keys_vect, dict, keys_dict))
}
