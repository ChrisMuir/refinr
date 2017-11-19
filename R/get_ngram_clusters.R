#' Get ngram clusters
#'
#' Create clusters of similar values based on the ngram fingerprints of those
#' values. For more info on ngram fingerprinting, see
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#' @param one_gram_keys Character vector of ngram fingerprints for which n
#'   equals one.
#' @param n_gram_keys Character vector of ngram fingerprints for which n is
#'   a user defined numeric value passed as a parameter to function
#'   \code{\link{n_gram_merge}}.
#' @param edit_threshold Numeric value indicating the threshold at which a
#'   merge is performed, based on the sum of the edit values derived from
#'   param \code{edit_dist_weights}. Default value is 1. If this parameter is
#'   set to 0 or NA, then no approximate string matching will be done, and all
#'   merging will be based on strings that have identical ngram fingerprints.
#' @param edit_dist_weights Numeric vector indicating the weights to assign to
#'   the four edit operations (see details below), for the purpose of
#'   approximate string matching. Default values are
#'   c(d = 0.33, i = 0.33, s = 1, t = 0.5). This parameter gets passed along
#'   to the function \code{\link[stringdist]{stringdistmatrix}}. Must be either
#'   a numeric vector of length four, or NA.
#'
#' @return A list, each element of which is a cluster of similar values.
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#' vect <- c("Acme Pizza, Inc.",
#'           "ACME PIZZA COMPANY",
#'           "Bob's Pizza",
#'           "bobs pizza, llc.",
#'           "Bobby's Pizza")
#' univect <- unique(vect)
#' one_gram_keys <- get_fingerprint_ngram(univect,
#'                                        numgram = 1,
#'                                        bus_suffix = TRUE)
#' n_gram_keys <- get_fingerprint_ngram(univect,
#'                                      numgram = 2,
#'                                      bus_suffix = TRUE)
#' get_ngram_clusters(one_gram_keys,
#'                    n_gram_keys,
#'                    edit_threshold = 1,
#'                    edit_dist_weights = c(d = 0.33,
#'                                          i = 0.33,
#'                                          s = 1,
#'                                          t = 0.5))
#' [[1]]
#' [[1]][[1]]
#' [1] "accmepizmepizazz" "accmepizmepizazz"
#'
#'
#' [[2]]
#' [[2]][[1]]
#' [1] "bobsizobpispzazz" "bobsizobpispzazz"
#'
#' # In this exmaple, "Bobby's Pizza" is ignored because it does not have a
#' # match.
#' }
get_ngram_clusters <- function(one_gram_keys,
                               n_gram_keys,
                               edit_threshold,
                               edit_dist_weights) {
  stopifnot(is.character(one_gram_keys) || is.null(one_gram_keys))
  stopifnot(is.character(n_gram_keys))
  stopifnot(is.numeric(edit_threshold) || is.na(edit_threshold))
  stopifnot(is.numeric(edit_dist_weights) || is.na(edit_dist_weights))

  if (is.na(edit_threshold) || edit_threshold == 0) {
    # If approximate string matching is disabled (via param 'edit_threshold'),
    # then find all elements of n_gram_keys that have one or more identical
    # matches. From that list, create clusters of n_gram_keys (groups that all
    # have an identical n_gram_key), eliminate all NA's within each group, and
    # eliminate all groups with length less than two, then return clusters.
    n_gram_keys_dups <- n_gram_keys %>%
      .[!is.na(.)] %>%
      .[cpp_duplicated(.)] %>%
      cpp_unique
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
  one_gram_keys_dups <- one_gram_keys %>%
    .[!is.na(.)] %>%
    .[cpp_duplicated(.)] %>%
    cpp_unique
  # If no duplicated keys exist, return NULL.
  if (length(one_gram_keys_dups) == 0) {
    return(NULL)
  }
  initial_clust <- get_ngram_initial_clusters(n_gram_keys,
                                              one_gram_keys,
                                              one_gram_keys_dups)

  # For each element of initial_clust, do a stringdist matrix and analyze the
  # closest match for each element (so if theres a cluster of n_gram_keys
  # that has 5 elements, then item 1 and 4 may be the best matches for each
  # other, items 2 and 3 may be best for each other, and item 5 may not have
  # a good match in the group).
  # First step, create a stringdistmatrix for every element of initial_clust.
  distmatrices <- lapply(
    initial_clust, function(x)
      stringdist::stringdistmatrix(x,
                                   weight = edit_dist_weights,
                                   useBytes = TRUE) %>%
      as.matrix %>%
      `dimnames<-`(NULL))

  # Next, for each matrix in distmatrices, create clusters of matches within
  # the matrix, based on lowest numeric edit distance (matches must have a
  # value below edit_threshold in order to be considered a cluster suitable
  # for merging).
  clusters <- filter_initial_clusters(distmatrices, edit_threshold,
                                      initial_clust)
  return(clusters)
}
