#' Value merging based on ngram fingerprints
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It uses a two step
#' process, the first is clustering values based on their ngram fingerprint
#' method, described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#' The second step is merging values based on approximate string matching of
#' the ngram fingerprints, using the \code{\link[stringdist]{stringdistmatrix}}
#' function from the package \code{\link{stringdist}}.
#'
#' @param vect Character vector of items for which you want similar values
#'   merged.
#' @param numgram Numeric value indicating the number of characters that
#'   will occupy each token. Default value is 2. This parameter gets passed
#'   along to the function \code{\link[ngram]{ngram}}.
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
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   working with a vector of business names it's recommended to set this to
#'   TRUE. Default value is TRUE.
#'
#' @details Parameter \code{edit_dist_weights} are edit distance values that
#'  get passed to the approximate string matching function
#'  \code{\link[stringdist]{stringdistmatrix}}. The param takes four arguments,
#'  each one is a specific type of edit, with default penalty value.
#'  \itemize{
#'  \item d: Deletion, default value is 0.33
#'  \item i: Insertion, default value is 0.33
#'  \item s: substitution, default value is 1
#'  \item t: transposition, default value is 0.5
#'  }
#'
#'  For parameter \code{numgram}, the function is unreliable for any values
#'  other than 2.
#'
#' @return Character vector with similar values merged.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' # The performance of the approximate string matching can be ajusted using
#' # parameter 'edit_dist_weights'.
#' x <- c("Acmme Pizza, Inc.",
#'        "ACME PIZA COMPANY",
#'        "Acme Pizzazza LLC")
#'
#' n_gram_merge(vect = x,
#'              numgram = 2,
#'              edit_threshold = 1,
#'              edit_dist_weights = c(d = 0.2, i = 1, s = 1, t = 1))
#'
#' n_gram_merge(vect = x,
#'              numgram = 2,
#'              edit_threshold = 1,
#'              edit_dist_weights = c(d = 1, i = 0.2, s = 1, t = 1))
#'
#' n_gram_merge(vect = x,
#'              numgram = 2,
#'              edit_threshold = 1,
#'              edit_dist_weights = c(d = 0.2, i = 0.2, s = 1, t = 1))
#'
#' @useDynLib refinr
#' @importFrom Rcpp sourceCpp
n_gram_merge <- function(vect,
                         numgram = 2,
                         edit_threshold = 1,
                         edit_dist_weights = c(d = 0.33,
                                               i = 0.33,
                                               s = 1,
                                               t = 0.5),
                         bus_suffix = TRUE) {
  stopifnot(is.character(vect))
  stopifnot(is.numeric(numgram))
  stopifnot(is.numeric(edit_threshold) || is.na(edit_threshold))
  stopifnot(is.logical(bus_suffix))
  if (!is.na(edit_dist_weights) &&
      !is.numeric(edit_dist_weights) &&
      length(edit_dist_weights) != 4) {
    stop("param 'edit_dist_weights' must be either a numeric vector with ",
         "length four, or NA")
  }
  if (!is.na(edit_threshold) && is.na(edit_dist_weights)) {
    stop("param 'edit_dist_weights' must not be NA if 'edit_threshold' ",
         "is not NA")
  }

  # If approx string matching is being used, then get ngram == 1 keys for all
  # records.
  ## TODO:  ----
  ## Add code to handle input numgram == 1 (can skip "n_gram_keys" below, and
  ## should skip a number of steps later in the function as well).
  univect <- unique(vect)
  if (!is.na(edit_threshold)) {
    one_gram_keys <- get_fingerprint_ngram(univect, numgram = 1, bus_suffix)
  } else {
    one_gram_keys <- NULL
  }
  # Get ngram == numgram keys for all records.
  n_gram_keys <- get_fingerprint_ngram(univect, numgram = numgram, bus_suffix)

  # Get clusters
  clusters <- get_ngram_clusters(one_gram_keys,
                                 n_gram_keys,
                                 edit_threshold,
                                 edit_dist_weights)

  # If no clusters were found, return vect unedited.
  if (is.null(clusters)) {
    return(vect)
  }

  # If approx string matching is being used, then eliminate elements of
  # clusters that are NA, then filter out repeating values within each cluster.
  if (!is.na(edit_threshold)) {
    clusters <- clusters %>%
      .[vapply(., function(x) any(!is.na(x)), logical(1))] %>%
      lapply(., function(x) {
        if (is.list(x)) {lapply(x, sort)} else {sort(x)}
      }) %>%
      lapply(., function(x) {
        if (is.list(x)) {lapply(x, unique)} else {unique(x)}
      })
  }

  # For each cluster, make mass edits to the values of vect related to that
  # cluster.
  if (length(clusters) > 0) {
    for (i in seq_len(length(clusters))) {
      if (is.list(clusters[[i]])) {
        for (k in clusters[[i]]) {
          k <- unlist(k, FALSE, FALSE)
          if (length(k) > 1) {
            vect <- merge_ngram_clusters_vector(k,
                                               n_gram_keys,
                                               univect,
                                               vect)
          } else {
            vect <- merge_ngram_clusters_string(k,
                                               n_gram_keys,
                                               univect,
                                               vect)
          }
        }
      } else {
        k <- clusters[[i]]
        if (length(k) > 1) {
          vect <- merge_ngram_clusters_vector(k,
                                             n_gram_keys,
                                             univect,
                                             vect)
        } else {
          vect <- merge_ngram_clusters_string(k,
                                             n_gram_keys,
                                             univect,
                                             vect)
        }
      }
    }
  }
  return(vect)
}
