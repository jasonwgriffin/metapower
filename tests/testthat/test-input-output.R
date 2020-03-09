context("User Input/fucntion output errors")

test_that("Check input error messages", {
  expect_error(mpower(runif(1,min=-100,max = 0),25, 10, 1.67, model = "random"),"Specifiy effect size in positive units")
  expect_error(mpower(.3,sample(-1000:0,1),10,1.67,model = "random"), "Sample size must be greater than 0")


})

test_that("check that output is numeric", {
  expect_match(class(mpower(.3,25,10,1.67,model = "random")), "numeric")

})
