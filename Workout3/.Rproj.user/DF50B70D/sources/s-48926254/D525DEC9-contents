getwd
source("~/Users/cameronmandley/Desktop/Workout3R/Workout3-Cameron-Mandley.R")

# context with one test that groups expectations
context("Checkers")

test_that("Checkers works as expected", {
  prob1 = -1
  prob2 = 2
  prob3 = 1/2
  trial1 = 0
  trial2 = -1
  trial3 = 5
  success1 = -1
  success2 = 6
  success3 = 3
  expect_error(check_prob(prob1))
  expect_error(check_prob(prob2))
  expect_equal(check_prob(prob3), TRUE)
  expect_equal(check_trials(trial1), TRUE)
  expect_error(check_trials(trial2))
  expect_error(check_success(success1, trial3))
  expect_error(check_success(success2, trial3))
  expect_equal(check_success(success3, trial3), TRUE)
})
