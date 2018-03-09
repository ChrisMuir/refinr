context("n_gram_merge")

vect <- c("Acmme Pizza, Inc.", "ACME PIZA COMPANY", "Acme Pizzazza LLC",
          "acme pizza limited", "Tom's Sports Equipment, Inc.",
          "toms sports equipment")
vect_ng <- n_gram_merge(vect)

test_that("output is a char vector", expect_is(vect_ng, "character"))

test_that("output lengths are correct", {
  expect_equal(length(vect_ng), length(vect))
  expect_equal(length(unique(vect_ng)), 2)
  expect_equal(length(unique(n_gram_merge(vect, numgram = 3))), 5)
  expect_equal(length(unique(n_gram_merge(vect, edit_threshold = 5))), 2)
  expect_equal(length(unique(n_gram_merge(vect, bus_suffix = FALSE))),
               length(vect))
  vect_ng <- n_gram_merge(
    vect, edit_dist_weights = c(d = 1, i = 1, s = 0.1, t = 0.1))
  expect_equal(length(unique(vect_ng)), 4)
})

test_that("param 'edit_threshold' set to NA having expected effect", {
  expect_equal(length(unique(n_gram_merge(vect, edit_threshold = NA))), 5)
})

vect <- c("Bakersfield Highschool", "BAKERSFIELD high",
          "high school, bakersfield")
vect_ng <- n_gram_merge(vect, ignore_strings = c("high", "school",
                                                 "highschool"))
test_that("param 'ignore_strings' having expected effect", {
  expect_equal(length(unique(vect_ng)), 1)
})

test_that("no errors when input includes len 1 str, len 0 str, NA's", {
  expect_equal(length(unique(n_gram_merge(c("cats", "CATS", "h")))), 2)
  expect_equal(length(unique(n_gram_merge(c("cats", "CATS", "")))), 2)
  expect_equal(sum(is.na(n_gram_merge(c("cats", "CATS", NA)))), 1)
})

test_that("ellipsis args are being handled properly", {
  vect <- c("Acmme Pizza, Inc.", "ACME PIZA COMPANY", "Acme Pizzazza LLC",
            "acme pizza limited", "Tom's Sports Equipment, Inc.",
            "toms sports equipment")
  vect_ng <- n_gram_merge(vect, method = "lv", useBytes = TRUE)
  expect_equal(length(unique(vect_ng)), 2)
  expect_error(n_gram_merge(vect, fakeArg = "some_value"))
  expect_error(n_gram_merge(vect, a = c("cats", "are", "great")))
  expect_error(n_gram_merge(vect, weight = c(1, 2, 1, 1)))
})
