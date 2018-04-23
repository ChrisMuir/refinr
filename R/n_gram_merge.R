#' Value merging based on ngram fingerprints
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It uses a two step
#' process, the first is clustering values based on their ngram fingerprint (described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}).
#' The second step is merging values based on approximate string matching of
#' the ngram fingerprints, using the [stringdistmatrix()] function from the
#' package \code{stringdist}.
#'
#' @param vect Character vector, items to be potentially clustered and merged.
#' @param numgram Numeric value, indicating the number of characters that
#'   will occupy each ngram token. Default value is 2.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
#' @param bus_suffix Logical, indicating whether the merging of records should
#'   be insensitive to common business suffixes or not. Default value is TRUE.
#' @param edit_threshold Numeric value, indicating the threshold at which a
#'   merge is performed, based on the sum of the edit values derived from
#'   param \code{weight}. Default value is 1. If this parameter is
#'   set to 0 or NA, then no approximate string matching will be done, and all
#'   merging will be based on strings that have identical ngram fingerprints.
#' @param weight Numeric vector, indicating the weights to assign to
#'   the four edit operations (see details below), for the purpose of
#'   approximate string matching. Default values are
#'   c(d = 0.33, i = 0.33, s = 1, t = 0.5). This parameter gets passed along
#'   to the \code{stringdist} function. Must be either
#'   a numeric vector of length four, or NA.
#' @param ... additional args to be passed along to \code{stringdist}.
#'
#' @details The values of arg \code{weight} are edit distance values that
#'  get passed to the \code{stringdist} edit distance function. The
#'  param takes four arguments, each one is a specific type of edit, with
#'  default penalty value.
#'  \itemize{
#'  \item d: Deletion, default value is 0.33
#'  \item i: Insertion, default value is 0.33
#'  \item s: substitution, default value is 1
#'  \item t: transposition, default value is 0.5
#'  }
#'
#' @return Character vector with similar values merged.
#' @export
#'
#' @examples
#' x <- c("Acme Pizza, Inc.", "ACME PIZA COMPANY", "Acme Pizzazza LLC")
#'
#' n_gram_merge(vect = x)
#'
#' # The performance of the approximate string matching can be ajusted using
#' # parameters 'weight' or 'edit_threshold'
#' n_gram_merge(vect = x,
#'              weight = c(d = 0.4, i = 1, s = 1, t = 1))
#'
#' # Use parameter 'ignore_strings' to ignore specific strings during merging
#' # of values.
#' x <- c("Bakersfield Highschool", "BAKERSFIELD high",
#'        "high school, bakersfield")
#' n_gram_merge(vect = x, ignore_strings = c("high", "school", "highschool"))
#'
n_gram_merge <- function(vect, numgram = 2, ignore_strings = NULL,
                         bus_suffix = TRUE, edit_threshold = 1,
                         weight = c(d = 0.33, i = 0.33, s = 1, t = 0.5), ...) {
  # Input validation.
  stopifnot(is.character(vect))
  stopifnot(is.numeric(numgram))
  stopifnot(is.numeric(edit_threshold) || is.na(edit_threshold))
  stopifnot(is.logical(bus_suffix))
  stopifnot(is.null(ignore_strings) || is.character(ignore_strings))
  if (!is.na(weight) && !is.numeric(weight) && length(weight) != 4) {
    stop("param 'weight' must be either a numeric vector with ",
         "length four, or NA", call. = FALSE)
  }
  if ((!is.na(edit_threshold) && edit_threshold == 0) ||
      numgram == 1) {
    edit_threshold <- NA
  }
  if (!is.na(edit_threshold) && is.na(weight)) {
    stop("param 'weight' must not be NA if 'edit_threshold'is not NA",
         call. = FALSE)
  }

  # If any args were passed via ellipsis, check to make sure they are valid
  # stringdist args.
  ellip_args <- names(list(...))
  if (any(c("a", "b") %in% ellip_args)) {
    stop("'stringdistmatrix' args 'a' and 'b' cannot be set manually",
         call. = FALSE)
  }
  sd_args <- get("sd_args", envir = refinr_env)
  if (!all(ellip_args %in% sd_args)) {
    bad_args <- paste(
      ellip_args[!ellip_args %in% sd_args],
      collapse = ", "
    )
    stop(paste("these input arg(s) are invalid:", bad_args), call. = FALSE)
  }

  # If approx string matching is being used, then get ngram == 1 keys for all
  # records.
  edit_threshold_missing <- is.na(edit_threshold)
  univect <- cpp_unique(vect[!is.na(vect)])
  if (!edit_threshold_missing) {
    one_gram_keys <- get_fingerprint_ngram(univect, numgram = 1, bus_suffix,
                                           ignore_strings)
  } else {
    one_gram_keys <- NULL
  }
  # Get ngram == numgram keys for all records.
  n_gram_keys <- get_fingerprint_ngram(univect, numgram = numgram, bus_suffix,
                                       ignore_strings)

  # If approximate string matching is not being used, return output of
  # ngram_merge_no_approx().
  if (edit_threshold_missing) {
    return(ngram_merge_no_approx(n_gram_keys, univect, vect))
  }

  # If approximate string matching is enabled, get initial clusters by finding
  # all elements of n_gram_keys for which their associated one_gram_key has
  # one or more matches within the entire list of one_gram_keys. Then create a
  # stringdistmatrix for each cluster, which will be used to filter the
  # initial clusters.

  # Get initial clusters.
  initial_clust <- get_ngram_initial_clusters(n_gram_keys, one_gram_keys)

  # Create a stringdistmatrix for every element of initial_clust.
  distmatrices <- lapply(initial_clust, function(x) {
    as_matrix(stringdistmatrix(x, weight = weight, ...))
  })

  # Filter clusters based on distmatrices, then for each cluster, make mass
  # edits to the values of vect related to that cluster.
  return(ngram_merge_approx(n_gram_keys, univect, vect, distmatrices,
                            edit_threshold, initial_clust))
}
