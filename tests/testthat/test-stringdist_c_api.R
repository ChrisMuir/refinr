context("list_lens")

test_list <- list(c(1, 2, 3), c("cats", "dogs"), c(), c(4L, 5L, 6L))

test_that("list_lens returns correct output",
          expect_equal(list_lens(test_list), c(3, 2, 0, 3)))
