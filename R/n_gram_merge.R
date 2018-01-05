#' Value merging based on ngram fingerprints
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It uses a two step
#' process, the first is clustering values based on their ngram fingerprint
#' method, described here
#' \url{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}.
#' The second step is merging values based on approximate string matching of
#' the ngram fingerprints, using the \code{stringdistmatrix} function from the
#' package \code{\link{stringdist}}.
#'
#' @param vect Character vector of items for which you want similar values
#'   merged.
#' @param numgram Numeric value indicating the number of characters that
#'   will occupy each ngram token. Default value is 2.
#' @param ignore_strings Character vector, these strings will be ignored during
#'   the merging of values within \code{vect}. Default value is NULL.
#' @param bus_suffix Logical indicating whether the merging of records should
#'   be insensitive to common business suffixes (TRUE) or not (FALSE). If
#'   input \code{vect} a vector of business names it's recommended to set this
#'   to TRUE. Default value is TRUE.
#' @param edit_threshold Numeric value indicating the threshold at which a
#'   merge is performed, based on the sum of the edit values derived from
#'   param \code{edit_dist_weights}. Default value is 1. If this parameter is
#'   set to 0 or NA, then no approximate string matching will be done, and all
#'   merging will be based on strings that have identical ngram fingerprints.
#' @param edit_dist_weights Numeric vector indicating the weights to assign to
#'   the four edit operations (see details below), for the purpose of
#'   approximate string matching. Default values are
#'   c(d = 0.33, i = 0.33, s = 1, t = 0.5). This parameter gets passed along
#'   to the \code{\link{stringdist}} function. Must be either
#'   a numeric vector of length four, or NA.
#' @param ... additional args to be passed along to \code{\link{stringdist}}.
#'
#' @details Parameter \code{edit_dist_weights} are edit distance values that
#'  get passed to the \code{\link{stringdist}} edit distance function. The
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
#' # parameters 'edit_dist_weights' or 'edit_threshold'
#' n_gram_merge(vect = x,
#'              edit_dist_weights = c(d = 0.4, i = 1, s = 1, t = 1))
#'
#' # Use parameter 'ignore_strings' to ignore specific strings during merging
#' # of values.
#' x <- c("Bakersfield Highschool", "BAKERSFIELD high",
#'        "high school, bakersfield")
#' n_gram_merge(vect = x, ignore_strings = c("high", "school", "highschool"))
#'
#' @useDynLib refinr
#' @importFrom Rcpp sourceCpp
n_gram_merge <- function(vect, numgram = 2, ignore_strings = NULL,
                         bus_suffix = TRUE, edit_threshold = 1,
                         edit_dist_weights = c(d = 0.33, i = 0.33, s = 1,
                                               t = 0.5), ...) {
  # Input validation.
  stopifnot(is.character(vect))
  stopifnot(is.numeric(numgram))
  stopifnot(is.numeric(edit_threshold) || is.na(edit_threshold))
  stopifnot(is.logical(bus_suffix))
  stopifnot(is.null(ignore_strings) || is.character(ignore_strings))
  if (!is.na(edit_dist_weights) && !is.numeric(edit_dist_weights) &&
      length(edit_dist_weights) != 4) {
    stop("param 'edit_dist_weights' must be either a numeric vector with ",
         "length four, or NA", call. = FALSE)
  }
  if ((!is.na(edit_threshold) && edit_threshold == 0) ||
      numgram == 1) {
    edit_threshold <- NA
  }
  if (!is.na(edit_threshold) && is.na(edit_dist_weights)) {
    stop("param 'edit_dist_weights' must not be NA if 'edit_threshold' ",
         "is not NA", call. = FALSE)
  }

  # If any args were passed via ellipsis, check to make sure they are valid
  # stringdist args.
  ellip_args <- names(list(...))
  if (any(c("a", "b") %in% ellip_args)) {
    stop("'stringdistmatrix' args 'a' and 'b' cannot be set manually",
         call. = FALSE)
  }
  if ("weight" %in% ellip_args) {
    stop("please use arg 'edit_dist_weight' in place of stringdistmatrix ",
         "arg 'weight'", call. = FALSE)
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
  univect <- cpp_unique(vect[!is.na(vect)])
  if (!is.na(edit_threshold)) {
    one_gram_keys <- get_fingerprint_ngram(univect, numgram = 1, bus_suffix,
                                           ignore_strings)
  } else {
    one_gram_keys <- NULL
  }
  # Get ngram == numgram keys for all records.
  n_gram_keys <- get_fingerprint_ngram(univect, numgram = numgram, bus_suffix,
                                       ignore_strings)

  ## Get clusters.
  if (is.na(edit_threshold)) {
    # If approximate string matching is disabled (via param 'edit_threshold'),
    # then find all elements of n_gram_keys that have one or more identical
    # matches. From that list, create clusters of n_gram_keys (groups that all
    # have an identical n_gram_key), eliminate all NA's within each group, and
    # eliminate all groups with length less than two, then return clusters.
    n_gram_keys_dups <- cpp_get_key_dups(n_gram_keys)
    # If no duplicated keys exist, return vect unedited.
    if (length(n_gram_keys_dups) == 0) return(vect)
    clusters <- as.list(n_gram_keys_dups)
  } else {
    # If approximate string matching is enabled, then find all elements of
    # n_gram_keys for which their associated one_gram_key has one or more
    # identical matches within the entire list of one_gram_keys. From that
    # list, create clusters of n_gram_keys (groups that all have an identical
    # one_gram_key), eliminate all NA's within each group, and eliminate all
    # groups with length less than two.
    one_gram_keys_dups <- cpp_get_key_dups(one_gram_keys)
    # If no duplicated keys exist, return vect unedited.
    if (length(one_gram_keys_dups) == 0) return(vect)
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
  }

  # If approx string matching is being used, then:
  # 1. Eliminate clusters that are all NA's.
  # 2. Filter out repeating values within each cluster.
  # 3. Flatten nested lists so that each element of clusters is a char vector.
  # 4. Get unique elements of the overall list.
  if (!is.na(edit_threshold)) {
    clusters <- clusters[vapply(clusters, function(x)
      any(!is.na(x)), logical(1))]
    # If clusters were all NA, return vect unedited.
    if (length(clusters) == 0) return(vect)
    clusters <- lapply(clusters, function(x)
      cpp_list_unique(x, sort_vals = TRUE))
    clusters <- unique(flatten_list(clusters))
  }

  # For each cluster, make mass edits to the values of vect related to that
  # cluster.
  vect <- merge_ngram_clusters(clusters, n_gram_keys, univect, vect)

  return(vect)
}
