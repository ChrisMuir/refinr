#' Value merging based on Key Collision
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It clusters values
#' based on the key collision method, described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector of items for which you want similar values
#'   merged and edited to be identical.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   input \code{vect} a vector of business names it's recommended to set this
#'   to TRUE. Default value is TRUE.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
#' @param dict Character vector, meant to act as a dictionary during the
#'   merging process. If any items within \code{vect} have a match in dict,
#'   then those items will always be edited to be identical to their match in
#'   dict. Optional param, and default value is NULL. If no dictionary is
#'   passed, then clusters will be created and merged within \code{vect}
#'   without the aid of dictionary values.
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
key_collision_merge <- function(vect, bus_suffix = TRUE, ignore_strings = NULL,
                                dict = NULL) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))
  stopifnot(is.null(dict) || is.character(dict))
  stopifnot(is.null(ignore_strings) || is.character(ignore_strings))

  # If dict is not NULL, get unique values of dict.
  if (!is.null(dict)) dict <- cpp_unique(dict)

  # If ignore_strings is not NULL, make all values lower case then get uniques.
  if (!is.null(ignore_strings)) {
    ignore_strings <- unique(tolower(ignore_strings))
  }

  # Get vector of key values. If dict is not NULL, get vector of key values
  # for dict as well.
  keys_vect <- get_fingerprint_KC(vect, bus_suffix, ignore_strings)
  if (!is.null(dict)) {
    keys_dict <- get_fingerprint_KC(dict, bus_suffix, ignore_strings)
  }

  # If dict is NULL, get vector of all key values that have at least one
  # duplicate within keys. Otherwise, get all key_vect values that have:
  # 1. At least one duplicate within key_vect, AND/OR
  # 2. At least one matching value within key_dict.
  if (is.null(dict)) {
    clusters <- cpp_get_key_dups(keys_vect)
  } else {
    clusters <- cpp_get_key_dups(c(keys_vect, keys_dict))
  }

  # For each cluster, make mass edits to the values of vect related to that
  # cluster.
  keys_in_clusters <- keys_vect %in% clusters
  if (is.null(dict)) {
    vect <- merge_KC_clusters_no_dict(clusters, keys_vect, vect,
                                      keys_in_clusters)
  } else {
    vect <- merge_KC_clusters_dict(clusters, keys_vect, vect, keys_dict, dict,
                                   keys_in_clusters)
  }
  return(vect)
}
