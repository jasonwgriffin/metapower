context("Check values match those from which the formulas were derived")

test_that("Check power test from Pigott 2012, page 43", {

  expect_equal(round(mpower(effect_size = .5,
                      sample_size = 20,
                      k = 10,
                      es_type ="d",
                      test_type = "one-tailed")$power$fixed_power,2), 1)

  # page 44
  expect_equal(round(mpower(effect_size = .2,
                            sample_size = 10,
                            k = 10,
                            es_type ="d")$power$fixed_power,2), .29)
  #page 46
  expect_equal(round(mpower(effect_size = 1.3,
                            sample_size = 2000,
                            k = 9,
                            es_type = "OR",
                            test_type = "one-tailed",
                            con_table = c(26,974,20,980))$power$fixed_power,2), .83)
  #50
  expect_equal(round(mpower(effect_size = .5,
                            sample_size = 20,
                            k = 10,
                            es_type = "d",
                            test_type = "one-tailed")$power$random_power_s,3), .996)
  expect_equal(round(mpower(effect_size = .5,
                            sample_size = 20,
                            k = 10,
                            es_type = "d",
                            test_type = "one-tailed")$power$random_power_l,2), .79)
})
