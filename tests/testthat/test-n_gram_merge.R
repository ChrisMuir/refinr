context("n_gram_merge")
#library(magrittr)

## Tests using valid and similar input values.
vect <- c("Acmme Pizza, Inc.",
          "ACME PIZA COMPANY",
          "Acme Pizzazza LLC",
          "acme pizza limited",
          "Tom's Sports Equipment, Inc.",
          "toms sports equipment")
vect_ng <- n_gram_merge(vect,
                        numgram = 2,
                        edit_threshold = 1,
                        edit_dist_weights = c(d = 0.2, i = 0.2, s = 1, t = 0.5),
                        bus_suffix = TRUE)

test_that("similar vals, output is a char vector", {
  expect_is(vect_ng, "character")
})

test_that("similar vals, output lengths are correct", {
  expect_equal(length(vect_ng), length(vect))
  expect_equal(length(unique(vect_ng)), 2)
  expect_equal(length(unique(n_gram_merge(vect, numgram = 3))), 5)
  expect_equal(length(unique(n_gram_merge(vect, edit_threshold = 5))), 2)
  expect_equal(length(unique(n_gram_merge(vect, bus_suffix = FALSE))),
               length(vect))

  vect_ng <- n_gram_merge(vect,
                          edit_dist_weights = c(d = 1,
                                                i = 1,
                                                s = 0.1,
                                                t = 0.1))
  expect_equal(length(unique(vect_ng)), 4)
})

vect <- c("Bakersfield Highschool", "BAKERSFIELD high", "high school, bakersfield")
ignore_strings <- c("high", "school", "highschool")
test_that("param 'ignore_strings' having expected effect", {
  expect_equal(
    unique(n_gram_merge(vect, ignore_strings = ignore_strings, bus_suffix = TRUE)),
    vect[2]
  )
})


## Tests using valid and dissimilar input values.
vect <- c("Acme Pizza, Inc.",
          "Bob's Breakfast Diner",
          "random_char_string",
          "Cats are the best")
vect_ng <- n_gram_merge(vect)

test_that("non-similar vals, output lengths are correct", {
  expect_equal(length(unique(vect_ng)), length(vect))
})


## Tests using invalid input values.
test_that("invalid input throws an error", {
  expect_error(n_gram_merge(c(1, 2, 3, 4)))
  expect_error(n_gram_merge(c("cats", "dogs"), bus_suffix = "invalid"))
  expect_error(n_gram_merge(c("cats", "dogs"), numgram = "invalid"))
  expect_error(n_gram_merge(c("cats", "dogs"), edit_threshold = "invalid"))
  expect_error(n_gram_merge(c("cats", "dogs"), edit_dist_weights = "invalid"))
})
