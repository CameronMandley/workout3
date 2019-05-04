getwd
source("~/Users/cameronmandley/Desktop/Workout3R/Workout3-Cameron-Mandley.R")

context("Binomial")

test_that("Binomial values are as expected", {
  n1 = 20
  k1 = 5
  prob1 = .25
  expect_equal(bin_choose(n1, k1), 15504)
  expect_error(bin_choose(k1, n1))
  expect_equal(bin_probability(k1, n1, prob1), 0.2023312)
})
