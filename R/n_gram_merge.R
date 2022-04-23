#' Value merging based on ngram fingerprints
#'
#' This function takes a character vector and makes edits and merges values
#' that are approximately equivalent yet not identical. It uses a two step
#' process, the first is clustering values based on their ngram fingerprint (described here
#' \url{https://docs.openrefine.org/next/technical-reference/clustering-in-depth/}).
#' The second step is merging values based on approximate string matching of
#' the ngram fingerprints, using the [sd_lower_tri()] C function from the
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
#' @param ... additional args to be passed along to the \code{stringdist}
#'   function. The acceptable args are identical to those of
#'   [stringdistmatrix()].
#'
#' @details The values of arg \code{weight} are edit distance values that
#'  get passed to the \code{stringdist} edit distance function. The
#'  param takes four arguments, each one is a specific type of edit, with
#'  default penalty value.
#'  \itemize{
#'  \item d: deletion, default value is 0.33
#'  \item i: insertion, default value is 0.33
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
  if (all(!is.na(weight))) {
    if (!is.numeric(weight) && length(weight) != 4) {
      stop("param 'weight' must be either a numeric vector with ",
           "length four, or NA", call. = FALSE)
    } else {
      weight <- as.double(weight)
    }
  }
  if ((!is.na(edit_threshold) && edit_threshold == 0) ||
      numgram == 1) {
    edit_threshold <- NA
  }
  edit_threshold_missing <- is.na(edit_threshold)
  if (!edit_threshold_missing && any(is.na(weight))) {
    stop("param 'weight' must not be NA if 'edit_threshold'is not NA",
         call. = FALSE)
  }

  # If any args were passed via ellipsis, check to make sure they are valid
  # stringdist args.
  dots <- list(...)
  dots_names <- names(dots)
  if (any(c("a", "b") %in% dots_names)) {
    stop("'stringdist' args 'a' and 'b' cannot be set manually",
         call. = FALSE)
  }
  # Vector of valid arg names for stringdist.
  sdm_args <- c("method", "useBytes", "weight", "q", "p", "bt", "useNames",
                "nthread")
  if (!all(dots_names %in% sdm_args)) {
    bad_args <- paste(
      dots_names[!dots_names %in% sdm_args],
      collapse = ", "
    )
    stop(paste("these input arg(s) are invalid:", bad_args), call. = FALSE)
  }

  # More input validations for stringdist args.
  if (!edit_threshold_missing) {
    if (!"method" %in% dots_names) {
      method <- 1L
    } else {
      sdm_methods <- c(osa = 0L, lv = 1L, dl = 2L, hamming = 3L, lcs = 4L,
                       qgram = 5L, cosine = 6L, jaccard = 7L, jw = 8L,
                       soundex = 9L)
      if (!dots$method %in% names(sdm_methods)) {
        stop(
          sprintf("arg 'method' must be one of:\n%s",
                  paste(names(sdm_methods), collapse = ", ")),
          call. = FALSE
        )
      }
      method <- sdm_methods[dots$method]
    }

    if (!"nthread" %in% dots_names) {
      nthread <- getOption("sd_num_thread")
    } else {
      stopifnot(is.numeric(dots$nthread) && dots$nthread > 0)
      nthread <- as.integer(dots$nthread)
    }

    if (!"useBytes" %in% dots_names) {
      useBytes <- FALSE
    } else {
      stopifnot(is.logical(dots$useBytes))
      useBytes <- dots$useBytes
    }

    if (!"q" %in% dots_names) {
      q <- 1
    } else {
      stopifnot(dots$q >= 0)
      q <- as.integer(dots$q)
    }

    if (!"p" %in% dots_names) {
      p <- 0
    } else {
      stopifnot(dots$p <= 0.25 && dots$p >= 0)
      p <- as.double(dots$p)
    }

    if (!"bt" %in% dots_names) {
      bt <- 0
    } else {
      stopifnot(is.numeric(dots$bt))
      bt <- as.double(dots$bt)
    }
  }

  # If ignore_strings is not NULL, make all values lower case then get uniques.
  if (!is.null(ignore_strings)) {
    ignore_strings <- unique(
      cpp_tolower(ignore_strings[!is.na(ignore_strings)])
    )
  }

  # If approx string matching is being used, then get ngram == 1 keys for all
  # records.
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

  # If approximate string matching is enabled, call ngram_merge_approx(). This
  # will do the following:
  # 1. Get initial clusters by finding all elements of n_gram_keys for which
  #    their associated one_gram_key has one or more matches within the entire
  #    list of one_gram_keys.
  # 2. Create a stringdistmatrix for every initial cluster, then filter
  #    clusters based on the dist matrices.
  # 3. For each remaining cluster, make mass edits to the values of vect
  #    related to that cluster. Return vect after mass edits have been made.
  ngram_merge_approx(n_gram_keys, one_gram_keys, univect, vect, edit_threshold,
                     method, weight, p, bt, q, useBytes, nthread)
}
