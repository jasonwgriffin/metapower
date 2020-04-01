context("User Input/function output errors")

test_that("Check input error messages", {
  ## Effect Size errors
  expect_error(mpower(effect_size = , sample_size = 10, k = 10, es_type = "d"), "Need to specify expected effect size")
  expect_error(mpower(effect_size = "one" , sample_size = 10, k = 10, es_type = "d"), "effect_size must be numeric")
  expect_error(mpower(effect_size = c(.5,.5), sample_size = 10, k = 10, es_type = "d"), "effect_size must be a single number")
  expect_warning(mpower(effect_size = 11, sample_size = 10, k = 10, es_type = "d"), "Are you sure effect size is >10?")
  ## sample sizes errors
  expect_error(mpower(effect_size = .5, sample_size = , k = 10, es_type = "d"), "Need to specify expected sample size")
  expect_error(mpower(effect_size = .5, sample_size = 0, k = 10, es_type = "d"), "sample_size must be greater than 0")
  expect_error(mpower(effect_size = .5, sample_size = "ten", k = 10, es_type = "d"), "sample_size must be numeric")
  expect_error(mpower(effect_size = .5, sample_size = c(10,10), k = 10, es_type = "d"), "sample_size must be a single number")
  ## num studies errors
  expect_error(mpower(effect_size = .5, sample_size = 10, k = , es_type = "d"), "Need to specify expected number of studies")
  expect_error(mpower(effect_size = .5, sample_size = 10, k = "ten", es_type = "d"), "k must be numeric")
  expect_error(mpower(effect_size = .5, sample_size = 10, k = c(10,10), es_type = "d"), "k must be a single number")
  expect_error(mpower(effect_size = .5, sample_size = 10, k = 1, es_type = "d"), "k must be greater than 1")
  ## es_type errors
  expect_error(mpower(effect_size = .5, sample_size = 10, k = 10, es_type = ), "Need to specify effect size as 'd', 'Correlation', or 'OR'")
  expect_error(mpower(effect_size = .5, sample_size = 10, k = 10, es_type = 1), "Need to specify effect size as 'd', 'Correlation', or 'OR'")
  ## test_type errors
  expect_error(mpower(effect_size = .5, sample_size = 10, k = 10, es_type = "d", test_type = 1), "Need to specify two-tailed or one-tailed")
})

test_that("check that output is an 'mpower' class", {
  expect_match(class(mpower(effect_size = .5, sample_size = 10, k = 10, es_type = "d")), "mpower")

})
