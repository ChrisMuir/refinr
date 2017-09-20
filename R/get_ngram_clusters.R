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
#'                    edit_dist_weights = c(d = 0.33, i = 0.33, s = 1, t = 0.5))
#' [[1]]
#' [[1]][[1]]
#' [1] "accmepizmepizazz" "accmepizmepizazz"
#'
#'
#' [[2]]
#' [[2]][[1]]
#' [1] "bobsizobpispzazz" "bobsizobpispzazz"
#' # In this exmaple, "Bobby's Pizza" is ignored because it does not have a
#' # match.
#' }
get_ngram_clusters <- function(one_gram_keys,
                               n_gram_keys,
                               edit_threshold = edit_threshold,
                               edit_dist_weights = edit_dist_weights) {
  stopifnot(is.character(one_gram_keys))
  stopifnot(is.character(n_gram_keys))
  stopifnot(is.numeric(edit_threshold) || is.na(edit_threshold))
  stopifnot(is.numeric(edit_dist_weights))

  if (is.na(edit_threshold) || edit_threshold == 0) {
    # If approximate string matching is disabled (via param 'edit_threshold'),
    # then find all elements of n_gram_keys that have one or more identical
    # matches. From that list, create clusters of n_gram_keys (groups that all
    # have an identical n_gram_key), eliminate all NA's within each group, and
    # eliminate all groups with length less than two, then return clusters.
    n_gram_keys_dups <- n_gram_keys %>%
      .[!is.na(.)] %>%
      .[duplicated(.)] %>%
      unique
    # If no duplicated keys exist, return NULL.
    if (length(n_gram_keys_dups) == 0) {
      return(NULL)
    }
    clusters <- lapply(n_gram_keys_dups, function(x) {
      n_gram_keys[which(equality(n_gram_keys, x))] %>%
        .[!is.na(.)]
    }) %>%
      .[vapply(., length, integer(1), USE.NAMES = FALSE) > 1]
    return(clusters)
  } else {
    # If approximate string matching is enabled, then find all elements of
    # n_gram_keys for which their associated one_gram_key has one or more
    # identical matches within the entire list of one_gram_keys. From that
    # list, create clusters of n_gram_keys (groups that all have an identical
    # one_gram_key), eliminate all NA's within each group, and eliminate all
    # groups with length less than two.
    one_gram_keys_dups <- one_gram_keys %>%
      .[!is.na(.)] %>%
      .[duplicated(.)] %>%
      unique
    # If no duplicated keys exist, return NULL.
    if (length(one_gram_keys_dups) == 0) {
      return(NULL)
    }
    initial_clust <- lapply(one_gram_keys_dups, function(x) {
      n_gram_keys[which(equality(one_gram_keys, x))] %>%
        .[!is.na(.)]
    }) %>%
      .[vapply(., length, integer(1), USE.NAMES = FALSE) > 1]

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

    # Next, for each element of distmatrices, create clusters of matches within
    # the matrix, based on lowest numeric edit distance.
    # (matches must have a value below edit_threshold in order to be considered
    # a cluster suitable for merging).
    clusters <- lapply(seq_len(length(distmatrices)), function(i) {
      # For each row of distmatrices[[i]], get the min value present. If none
      # are below 1, then function will return NA and move to the next iter.
      lows <- vapply(seq_len(nrow(distmatrices[[i]])), function(x)
        min(distmatrices[[i]][x, -x]), numeric(1))
      if (any(lows < edit_threshold)) {
        # ID whether or not any of the rows of distmatrices[[i]] have more than
        # one cluster match (ie a min value that repeats within any given row).
        olap <- vapply(seq_len(length(lows)), function(x)
          if (lows[x] > edit_threshold) {return(0)}
          else {sum(distmatrices[[i]][x, -x] == lows[x])}, numeric(1))
        # If any rows of distmatrices[[i]] have a min edit distance that
        # repeats, then code below will generate the clusters, and then
        # eliminate pair-wise clusters that appear in any clusters of length
        # greater than 2.
        if (any(olap > 1)) {
          # Generate clusters.
          clust <- lapply(which(lows < edit_threshold), function(x)
            initial_clust[[i]][which(distmatrices[[i]][x, ] < edit_threshold)]) %>%
            unique
          # ID the cluster(s) that have the longest length.
          maxclust <- which.max(vapply(clust, length, numeric(1)))

          # clust_bool is a logical vector with the same length as clust,
          # indicating which clusters to keep (pair-wise clusters that appear
          # in longer clusters are not kept).
          clust_bool <- vapply(clust, function(k) {
            if (any(vapply(
              clust[maxclust], function(x) all(x %in% k), logical(1)))) {
              TRUE
            } else {
              all(vapply(clust[maxclust], function(x) !all(k %in% x),
                         logical(1)))
            }
          }, logical(1))
          return(clust[clust_bool])
        } else {
          return(
            lapply(which(lows < edit_threshold), function(x)
              initial_clust[[i]][which(distmatrices[[i]][x, ] < edit_threshold)]) %>%
              unique
          )
        }
      } else {
        return(NA)
      }
    })
    return(clusters)
  }
}
