#' Create clusters of similar values based on the ngram fingerprints of those
#' values.
#'
#' @noRd
get_ngram_clusters <- function(one_gram_keys, n_gram_keys, edit_threshold,
                               edit_dist_weights, ...) {
  stopifnot(is.character(one_gram_keys) || is.null(one_gram_keys))
  stopifnot(is.character(n_gram_keys))
  if (is.na(edit_threshold) || edit_threshold == 0) {
    # If approximate string matching is disabled (via param 'edit_threshold'),
    # then find all elements of n_gram_keys that have one or more identical
    # matches. From that list, create clusters of n_gram_keys (groups that all
    # have an identical n_gram_key), eliminate all NA's within each group, and
    # eliminate all groups with length less than two, then return clusters.
    n_gram_keys_dups <- cpp_get_key_dups(n_gram_keys)
    # If no duplicated keys exist, return NULL.
    if (length(n_gram_keys_dups) == 0) {
      return(NULL)
    } else {
      return(as.list(n_gram_keys_dups))
    }
  }

  # If approximate string matching is enabled, then find all elements of
  # n_gram_keys for which their associated one_gram_key has one or more
  # identical matches within the entire list of one_gram_keys. From that
  # list, create clusters of n_gram_keys (groups that all have an identical
  # one_gram_key), eliminate all NA's within each group, and eliminate all
  # groups with length less than two.
  one_gram_keys_dups <- cpp_get_key_dups(one_gram_keys)
  # If no duplicated keys exist, return NULL.
  if (length(one_gram_keys_dups) == 0) return(NULL)
  # Get initial clusters.
  initial_clust <- get_ngram_initial_clusters(n_gram_keys, one_gram_keys,
                                              one_gram_keys_dups)

  # Create a stringdistmatrix for every element of initial_clust.
  distmatrices <- lapply(initial_clust, function(x) {
    x <- as.matrix(
      stringdist::stringdistmatrix(x, weight = edit_dist_weights, ...)
    )
    dimnames(x) <- NULL
    x
  })

  # For each matrix in distmatrices, create clusters of matches within the
  # matrix, based on lowest numeric edit distance (matches must have a value
  # below edit_threshold in order to be considered suitable for merging).
  clusters <- filter_initial_clusters(distmatrices, edit_threshold,
                                      initial_clust)
  return(clusters)
}
