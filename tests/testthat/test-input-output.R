context("User Input/fucntion output errors")

test_that("Check input error messages", {
  ## Effect Size errors
  expect_error(mpower(sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "Need to specify expected effect size")
  expect_warning(mpower(effect_size = 11,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "Are you sure effect size is >10?")
  expect_error(mpower(effect_size = ".5", sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "effect_size must be numeric")
  expect_error(mpower(effect_size = c(.3,.7), sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "effect_size must be a single number")
  ## sd errors
  expect_error(mpower(effect_size = 1, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "Need to specify expected standard deviation of effect sizes")
  expect_warning(mpower(effect_size = 1, sd = 11, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "Are you sure standard deviation is >10?")
  expect_error(mpower(effect_size = 1, sd = ".5", sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "sd must be numeric")
  expect_error(mpower(effect_size = 1, sd = c(.3,.7), sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random"), "sd must be a single number")
  ## sample sizes errors
  expect_error(mpower(effect_size = 1, sd = .5, k = 10, hg = "small", es_type = "d", model = "random"), "Need to specify expected sample size")
  expect_error(mpower(effect_size = 1, sd = .5, sample_size = 0, k = 10, hg = "small", es_type = "d", model = "random"), "sample_size must be greater than 0")
  expect_error(mpower(effect_size = 1, sd = .5, sample_size = "20", k = 10, hg = "small", es_type = "d", model = "random"), "sample_size must be numeric")
  expect_error(mpower(effect_size = 1, sd = .5, sample_size = c(20,40), k = 10, hg = "small", es_type = "d", model = "random"), "sample_size must be a single number")
  ## num studies errors
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, hg = "small", es_type = "d", model = "random"), "Need to specify expected number of studies")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = "10", hg = "small", es_type = "d", model = "random"), "k must be numeric")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = c(10,30), hg = "small", es_type = "d", model = "random"), "k must be a single number")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 1, hg = "small", es_type = "d", model = "random"), "k must be greater than 1")
  ## es_type errors
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", model = "random"),"Need to specify effect size as 'd', 'Correlation', or 'OR'")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "z-score", model = "random"),"Need to specify effect size as 'd', 'Correlation', or 'OR'")
  ## test_type errors
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random", test_type = 1),"Need to specify two-tailed or one-tailed")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random", test_type = "two"),"Need to specify two-tailed or one-tailed")
  ## model errors
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d"),"Need to specify 'fixed' or 'random' effects model")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = 2),"Need to specify 'fixed' or 'random' effects model")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "rand"),"Need to specify 'fixed' or 'random' effects model")
  ##heterogenity errors
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, es_type = "d", model = "random"), "Need to specify small, medium, or large heterogenity")
  expect_error(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "moderate", es_type = "d", model = "random"), "Need to specify small, medium, or large heterogenity")
})

test_that("check that output is an 'mpower' class", {
  expect_match(class(mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random")), "mpower")

})
#mpower(effect_size = 1,sd = .5, sample_size = 20, k = 10, hg = "small", es_type = "d", model = "random")
