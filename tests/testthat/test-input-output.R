context("User Input/function output errors")

test_that("Check input error messages for mpower()", {
  ## Effect Size errors
  expect_error(mpower(effect_size = , study_size = 10, k = 10, i2 = .50, es_type = "d"), "Need to specify expected effect size")
  expect_error(mpower(effect_size = "one" , study_size = 10, k = 10, i2 = .50, es_type = "d"), "effect_size must be numeric")
  expect_error(mpower(effect_size = c(.5,.5), study_size = 10, k = 10, i2 = .50, es_type = "d"), "effect_size must be a single number")
  expect_warning(mpower(effect_size = 11, study_size = 10, k = 10, i2 = .50, es_type = "d"), "Are you sure effect size is >10?")
  ## sample sizes errors
  expect_error(mpower(effect_size = .5, study_size = , k = 10, i2 = .50, es_type = "d"), "Need to specify expected sample size")
  expect_error(mpower(effect_size = .5, study_size = 0, k = 10, i2 = .50, es_type = "d"), "study_size must be greater than 0")
  expect_error(mpower(effect_size = .5, study_size = "ten", k = 10, i2 = .50, es_type = "d"), "study_size must be numeric")
  expect_error(mpower(effect_size = .5, study_size = c(10,10), k = 10, i2 = .50, es_type = "d"), "study_size must be a single number")
  ## num studies errors
  expect_error(mpower(effect_size = .5, study_size = 10, k = , i2 = .50, es_type = "d"), "Need to specify expected number of studies")
  expect_error(mpower(effect_size = .5, study_size = 10, k = "ten", i2 = .50, es_type = "d"), "k must be numeric")
  expect_error(mpower(effect_size = .5, study_size = 10, k = c(10,10), i2 = .50, es_type = "d"), "k must be a single number")
  expect_error(mpower(effect_size = .5, study_size = 10, k = 1, i2 = .50, es_type = "d"), "k must be greater than 1")
  ## es_type errors
  expect_error(mpower(effect_size = .5, study_size = 10, k = 10, i2 = .50, es_type = ), "Need to specify effect size as 'd', 'r', or 'or'")
  expect_error(mpower(effect_size = .5, study_size = 10, k = 10, i2 = .50, es_type = 1), "Need to specify effect size as 'd', 'r', or 'or'")
  ## test_type errors
  expect_error(mpower(effect_size = .5, study_size = 10, k = 10, i2 = .50, es_type = "d", test_type = 1), "Need to specify two-tailed or one-tailed")
})

test_that("check that output is an 'mpower' class", {
  expect_match(class(mpower(effect_size = .5, study_size = 10, k = 10, i2 = .50, es_type = "d")), "mpower")

})

test_that("Check input error messages for subgroup_power()", {
  ## Effect Size errors
  expect_error(subgroup_power(n_groups = 2, effect_sizes = .5, study_size = 20, k = 20, i2 = .5, es_type = "d"), "The number of of effect sizes should match the number of groups")
  expect_error(subgroup_power(n_groups = 2, effect_sizes = c(".1", ".5"), study_size = 20, k = 20, i2 = .5, es_type = "d"), "Effect sizes must be  numeric")

  ## sample size errors
  expect_error(subgroup_power(n_groups = 2, effect_sizes = c(.1,.5), study_size = c(20,20), k = 20, i2 = .5, es_type = "d"), "study_size must be a single number")
  expect_error(subgroup_power(n_groups = 2, effect_sizes = c(.1,.5), study_size = "twenty", k = 20, i2 = .5, es_type = "d"), "study_size must be numeric")


  # metric errors
  expect_error(subgroup_power(n_groups = 2, effect_sizes = c(.1,.5), study_size = 20, k = 20, i2 = .5, es_type = "or"), "For Odds Ratio, only enter the 2x2 contingency table. Remove effect_size argument")
  expect_error(subgroup_power(n_groups = 2, study_size = 20, k = 20, i2 = .5, es_type = "or", con_table = c(5,5,5,5)), "con_table should be input as a list with each element reflect the group 2x2 tables")
  expect_error(subgroup_power(n_groups = 2, study_size = 20, k = 20, i2 = .5, es_type = "or", con_table = list(g = c(5,5,5,5))), "Only 1 group 2x2 table is specified")
  expect_error(subgroup_power(n_groups = 2, con_table = list(g1 = c(6,5,4,5), g2 = c("hello")), study_size = 20, k = 20, i2 = .5, es_type = "or"),"Each element of con_table should be numeric")
})

test_that("check that output is an 'subgroup_power' class", {
  expect_match(class(subgroup_power(n_groups = 2, con_table = list(g1 = c(6,5,4,5), g2 = c(8,5,2,5)), study_size = 40, k = 20, i2 = .5, es_type = "or")), "subgroup_power")

})


