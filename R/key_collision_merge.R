#' Value merging based on Key Collision
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It clusters values
#' based on the key collision method, described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector of items for which you want similar values
#'   merged.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   working with a vector of business names it's recommended to set this to
#'   TRUE. Default value is TRUE.
#'
#' @return Character vector with similar values merged.
#' @export
#'
#' @examples \dontrun{
#' x <- c("Acme Pizza, Inc.",
#'        "ACME PIZZA COMPANY",
#'        "pizza, acme llc",
#'        "Acme Pizza, Inc.")
#' key_collision_merge(vect = x, bus_suffix = TRUE)
#' [1] "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc."
#' }
key_collision_merge <- function(vect, bus_suffix = TRUE) {
  stopifnot(is.character(vect))
  stopifnot(is.logical(bus_suffix))

  # Apply func get_fingerprint_KC to the input vector, generating a vector
  # of key values.
  keys <- get_fingerprint_KC(vect, bus_suffix)

  # Get vector of all key values that have at least one duplicate within keys.
  clusters <- unique(keys[duplicated(keys)])
  clusters <- clusters[!is.na(clusters)]

  # Create subsets of vect and keys based on which elements of each contain
  # at least one duplicate.
  vectsub <- vect[which(keys %in% clusters)]
  keyssub <- keys[keys %in% clusters]

  # For each element of clusters, get the number of unique values within vect
  # associated with that cluster. Idea is to skip the merging step for all
  # elements of cluster for which each associated element of vect is already
  # identical (in those spots its pointless to perform merging).
  csize <- vapply(clusters, function(n) {
    vectsub[which(equality(lookupvect = keyssub, charstring = n))] %>%
      unique %>%
      length
  },
  integer(1),
  USE.NAMES = FALSE)

  # Perform merging on all clusters with length greater than one.
  if (any(csize > 1)) {
    ids <- clusters[which(csize > 1)]
    vect <- merge_clusters(ids, keys, vect, keyssub, vectsub)
  } else {
    return(vect)
  }
  return(vect)
}
