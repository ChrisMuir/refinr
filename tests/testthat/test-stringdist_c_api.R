context("stringdist C API")

test_list <- list(c(1, 2, 3), c("cats", "dogs"), c(), c(4L, 5L, 6L))

test_that("Rcpp_str_dist_lengths returns correct output",
          expect_equal(Rcpp_str_dist_lengths(test_list), c(3, 2, 0, 3)))

test_that("Rcpp_str_dist_all_int returns correct output",
          expect_equal(Rcpp_str_dist_all_int(test_list), FALSE))
