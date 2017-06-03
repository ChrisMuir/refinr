context("key_collision_merge")

## Tests using valid and similar input values.
vect <- c("Acme Pizza, Inc.",
          "Acme Pizza, Inc.",
          "ACME PIZZA COMPANY",
          "acme pizza LLC")
dict <- c("Nicks Pizza", "acme PIZZA inc")
vect_kc <- key_collision_merge(vect, bus_suffix = TRUE)

test_that("similar vals, output is a char vector", {
  expect_is(vect_kc, "character")
})

test_that("similar vals, output lengths are correct", {
  expect_equal(length(vect_kc), length(vect))
  expect_equal(length(unique(vect_kc)), 1)
  expect_equal(
    length(unique(key_collision_merge(vect, bus_suffix = FALSE))), 3
  )
})

test_that("param 'dict' having expected effect", {
  expect_equal(unique(key_collision_merge(vect, dict = dict, bus_suffix = TRUE)), dict[2])
})


## Tests using valid and dissimilar input values.
vect <- c("Acme Pizza, Inc.",
          "Bob's Breakfast Diner",
          "random_char_string",
          "Cats are the best")
vect_kc <- key_collision_merge(vect)

test_that("non-similar vals, output lengths are correct", {
  expect_equal(length(vect_kc), length(vect))
})


## Tests using invalid input values.
test_that("invalid input throws an error", {
  expect_error(key_collision_merge(c(1, 2, 3, 4)))
  expect_error(key_collision_merge(c("cats", "dogs"), bus_suffix = "true"))
})
