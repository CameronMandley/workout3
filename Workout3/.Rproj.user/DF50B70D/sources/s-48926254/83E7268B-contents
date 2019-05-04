getwd
source("~/Users/cameronmandley/Desktop/Workout3R/Workout3-Cameron-Mandley.R")

context("Summary measures")

test_that("Summary measures are as expected", {
  trials = 20
  prob = .25
  expect_equal(aux_mean(trials, prob), 5)
  expect_equal(aux_variance(trials, prob), 3.75)
  expect_equal(aux_mode(trials, prob), 5)
  expect_equal(aux_skewness(trials, prob), 0.2581989)
  expect_equal(aux_kurtosis(trials, prob), -0.03333333)
})
