context("User Input/fucntion output errors")

test_that("Check input error messages", {
  expect_error(mpower(-11,25, 10, 1.67, model = "random"),"Specifiy effect size in positive units")
  expect_error(mpower(.3,-1,10,1.67,model = "random"), "Sample size must be greater than 0")
  expect_error(mpower(.3,25,-3,1.67,model = "random"), "Number of studies must be greater than 0")
  expect_error(mpower(.3,25,10,1.67), "Need to specify type of model")
  expect_error(mpower(.3,25,10,1.67, model = "rand"), "Need to specify 'fixed' or 'random' effects model")
  expect_error(mpower(.3,25,10,1.67, model = "random", test_type = "one"), "Need to specify one-tailed or two-tailed")



})

test_that("check that output is an 'mpower' class", {
  expect_match(class(mpower(.3,25,10,1.67,model = "random")), "mpower")

})
