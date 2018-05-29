# Function that attempts to merge common business name suffixes within a
# character string.
business_suffix <- function(vect) {
  vect <- gsub(" incorporated| incorporate", " inc", vect, perl = TRUE)
  vect <- gsub(" corporation| corporations", " corp", vect, perl = TRUE)
  vect <- gsub(" company| companys| companies", " co", vect, perl = TRUE)
  vect <- gsub(" limited liability co", " llc", vect, fixed = TRUE)
  vect <- gsub(" limited$", " ltd", vect, perl = TRUE)
  vect <- gsub(" division| divisions", " div", vect, perl = TRUE)
  vect <- gsub(" enterprises| enterprise", " ent", vect, perl = TRUE)
  vect <- gsub(" limited partnership", " lp", vect, fixed = TRUE)
  return(vect)
}

# Modified version of stats:::as.matrix.dist() that doesn't mess with dimnames.
as_matrix <- function (x) {
  size <- attr(x, "Size")
  df <- matrix(0, size, size)
  df[row(df) > col(df)] <- x
  df + t.default(df)
}

# R wrapper for stringdist C function `R_lower_tri()`.`
lower_tri <- function(a, method, weight, p, bt, q, useBytes, nthread) {
  x <- .Call("sd_lower_tri", a, method, weight, p, bt, q, useBytes, nthread)
  attributes(x) <- list(class = "dist", Size = length(a), Diag = TRUE,
                        Upper = TRUE, method = method)
  x
}

# vector for int-switch used by lower_tri()
sdm_methods <- c(osa = 0L, lv = 1L, dl = 2L, hamming = 3L, lcs = 4L,
                 qgram = 5L, cosine = 6L, jaccard = 7L, jw = 8L, soundex = 9L)

#' Get Length of Each List Element
#'
#' R wrapper for C function `Rcpp_str_dist_lengths()` (`Rcpp_str_dist_lengths()`
#' calls stringdist C function `R_lengths()`).
#'
#' @param x List
#'
#' @return Integer vector, indicating the length of each input list element.
#' @export
#'
#' @examples
#' test_list <- list(c(1, 2, 3), c("cats", "dogs"))
#' Rcpp_str_dist_lengths(test_list)
Rcpp_str_dist_lengths <- function(x) {
  Rcpp_test_lengths(x)
}

#' Check to see if all elements of a list are integer vectors
#'
#' R wrapper for C function `Rcpp_test_all_int()` (`Rcpp_test_all_int()`
#' calls stringdist C function `R_all_int()`)
#'
#' @param x List
#'
#' @return Logical
#' @export
#'
#' @examples
#' test_list <- list(c(1, 2, 3), c("cats", "dogs"))
#' Rcpp_str_dist_all_int(test_list)
Rcpp_str_dist_all_int <- function(x) {
  Rcpp_test_all_int(x)
}
