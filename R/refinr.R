#' @title Cluster and Merge Similar Values Within a Character Vector
#'
#' @description These functions take a character vector as input, identify and
#'  cluster similar values, and then merge clusters together so their values
#'  become identical. The functions are an implementation of the key collision
#'  and ngram fingerprint algorithms from the open source tool Open Refine.
#'
#' @section Documentation for Open Refine:
#'
#' \itemize{
#'   \item Open Refine Site \url{https://openrefine.org/}
#'   \item Details on Open Refine clustering algorithms \url{https://openrefine.org/docs/technical-reference/clustering-in-depth}
#' }
#'
#' @section Development links:
#'
#' \itemize{
#'   \item \url{https://github.com/ChrisMuir/refinr}
#'   \item Report bugs at \url{https://github.com/ChrisMuir/refinr/issues}
#' }
#'
#' @section \code{refinr} features the following functions:
#' \itemize{
#'   \item \code{\link{key_collision_merge}}
#'   \item \code{\link{n_gram_merge}}
#' }
#'
#' @useDynLib refinr
#' @import stringdist
#' @importFrom Rcpp sourceCpp
#' @importFrom stringi stri_trans_general
#' @docType package
#' @name refinr
"_PACKAGE"
