context("key_collision_merge")

vect <- c("Acme Pizza, Inc.", "Acme Pizza, Inc.", "ACME PIZZA COMPANY",
          "acme pizza LLC")
vect_kc <- key_collision_merge(vect)

test_that("similar vals, output is a char vector",
          expect_is(vect_kc, "character"))

test_that("similar vals, output lengths are correct", {
  expect_equal(length(vect_kc), length(vect))
  expect_equal(length(unique(vect_kc)), 1)
  expect_equal(
    length(unique(key_collision_merge(vect, bus_suffix = FALSE))), 3)
})

dict <- c("Nicks Pizza", "acme PIZZA inc")
test_that("param 'dict' having expected effect", {
  expect_equal(unique(key_collision_merge(vect, dict = dict)), dict[2])
})

vect <- c("Bakersfield Highschool", "BAKERSFIELD high",
          "high school, bakersfield")
vect_ng <- key_collision_merge(vect, ignore_strings = c("high", "school",
                                                        "highschool"))
test_that("param 'ignore_strings' having expected effect", {
  expect_equal(length(unique(vect_ng)), 1)
})

vect <- c("César Moreira Nuñez", "cesar moreira nunez")
test_that("encoding of input strings handled correctly",
          expect_equal(length(unique(key_collision_merge(vect))), 1))

vect <- c("cats", "CATS", "NA", "na", "na na", NA, NA_character_, " ", "")
test_that("NA values are handled correctly", {
  expect_equal(sum(is.na(key_collision_merge(vect))), 2)
})
