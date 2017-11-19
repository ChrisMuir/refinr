#' Value merging based on Key Collision
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It clusters values
#' based on the key collision method, described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#'
#' @param vect Character vector of items for which you want similar values
#'   merged and edited to be identical.
#' @param dict Character vector, meant to act as a dictionary during the
#'   merging process. If any items within \code{vect} have a match in dict,
#'   then those items will always be edited to be identical to their match in
#'   dict. Optional param, and default value is NULL. If no dictionary is
#'   passed, then clusters will be created and merged within \code{vect}
#'   without the aid of dictionary values.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   working with a vector of business names it's recommended to set this to
#'   TRUE. Default value is TRUE.
#'
#' @return Character vector with similar values merged.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' x <- c("Acme Pizza, Inc.",
#'        "ACME PIZZA COMPANY",
#'        "pizza, acme llc",
#'        "Acme Pizza, Inc.")
#' key_collision_merge(vect = x)
#'
#' # Add param "dict".
#' dict <- c("Nicks Pizza", "acme PIZZA inc")
#' key_collision_merge(vect = x, dict = dict)
#'
key_collision_merge <- function(vect, dict = NULL, bus_suffix = TRUE) {
  stopifnot(is.character(vect))
  stopifnot(is.null(dict) || is.character(dict))
  stopifnot(is.logical(bus_suffix))

  # If dict is not NULL, get unique values of dict.
  if (!is.null(dict)) dict <- cpp_unique(dict)

  # Apply func get_fingerprint_KC to the input vector, generating a vector
  # of key values. If dict is not NULL, get vector of key values for dict as
  # well.
  keys_vect <- get_fingerprint_KC(vect, bus_suffix)
  if (!is.null(dict)) keys_dict <- get_fingerprint_KC(dict, bus_suffix)

  # If dict is NULL, get vector of all key values that have at least one
  # duplicate within keys. Otherwise, get all key_vect values that have:
  # 1. At least one duplicate within key_vect, AND/OR
  # 2. At least one matching value within key_dict.
  if (is.null(dict)) {
    clusters <- cpp_get_key_dups(keys_vect)
  } else {
    clusters <- keys_vect[Reduce("|", list(keys_vect %in% keys_dict,
                                           cpp_duplicated(keys_vect)))] %>%
      cpp_unique %>%
      .[!is.na(.)]
  }

  # Create subsets of vect and keys_vect based on which elements of each contain
  # at least one duplicate.
  keys_in_clusters <- keys_vect %in% clusters
  vect_sub <- vect[keys_in_clusters]
  keys_vect_sub <- keys_vect[keys_in_clusters]

  # If dict is NULL, for each element of clusters, get the number of unique
  # values within vect associated with that cluster. Otherwise, for each
  # element of clusters, get the number of unique values across both vect AND
  # dict associated with that cluster. Idea is to skip the merging step for all
  # elements of cluster for which each associated element of vect is already
  # identical (or identical to an element of dict). In those spots its
  # pointless to perform merging.
  if (length(vect) == length(cpp_unique(vect))) {
    csize <- rep.int(2, length(clusters))
  } else {
    if (is.null(dict)) {
      csize <- get_clust_size_no_dict(clusters, vect_sub, keys_vect_sub)
    } else {
      csize <- get_clust_size_dict(clusters, vect_sub, keys_vect_sub, dict,
                                   keys_dict)
    }
  }

  # Perform merging on all clusters with length greater than one.
  if (any(csize > 1)) {
    clusters <- clusters[csize > 1]
    if (is.null(dict)) {
      vect <- merge_KC_clusters_no_dict(clusters, keys_vect, vect,
                                        keys_vect_sub,vect_sub)
    } else {
      vect <- merge_KC_clusters_dict(clusters, keys_vect, vect,
                                     keys_vect_sub, vect_sub, keys_dict,
                                     dict)
    }
  }
  return(vect)
}
