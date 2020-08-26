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
                            test_type = "one-tailed")$power$random_power_25,3), .993)
  expect_equal(round(mpower(effect_size = .5,
                            sample_size = 20,
                            k = 10,
                            es_type = "d",
                            test_type = "one-tailed")$power$random_power_75,2), .79)
  ## moderator analysis

  ## Page 71 - fixed power between study
  expect_equal(round(mod_power(n_groups = 3,
                               effect_sizes = c(.1,.2,.6),
                               sample_size = 15,
                               k = 15,
                               es_type = "Correlation",
                               test_type = "one-tailed")$mod_power$fixed_power_b,2), .83)
  ## page 74 - fixed power within study
  expect_equal(round(mod_power(n_groups = 3,
                               effect_sizes = c(.1,.2,.6),
                               sample_size = 15,
                               k = 15,
                               es_type = "Correlation",
                               sd_within = c(1,1,4),
                               test_type = "one-tailed")$mod_power$fixed_power_w,2), .83)

  ## Page 78 - random power (small heterogeneity)
  expect_equal(round(mod_power(n_groups = 3,
                               effect_sizes = c(.1,.2,.6),
                               sample_size = 15,
                               k = 15,
                               es_type = "Correlation",
                               sd_within = c(1,1,4),
                               test_type = "one-tailed")$mod_power$random_power_b_s,2), .05)

  ## Page 353 Pigott 2020

  expect_equal(round(mod_power(n_groups = 2,
                               effect_sizes = c(0,.5),
                               sample_size = 10,
                               k = 10,
                               es_type = "d",
                               #sd_within = c(1,1,4),
                               test_type = "one-tailed")$mod_power$fixed_power_b,2), .70)
  ## 359
  expect_equal(round(mod_power(n_groups = 2,
                               effect_sizes = c(0,.5),
                               sample_size = 20,
                               k = 10,
                               es_type = "d",
                               #sd_within = c(1,1,4),
                               test_type = "one-tailed")$mod_power$fixed_power_b,2), .70)

  expect_equal(round(mod_power(n_groups = 2,
                               effect_sizes = c(0,.5),
                               sample_size = 20,
                               k = 10,
                               es_type = "d",
                               test_type = "one-tailed")$mod_power$random_power_b_m,2), .42)

#
#   n3 <- mod_power(n_groups = 2,
#             #effect_sizes = c(1.5,1.7),
#             sample_size = 4000,
#             k = 10,
#             es_type = "OR",
#             con_table = list(g1 = c(1463,1000,537,1000),
#                              g2 = c(1000,1000,1000,1000)),
#             #sd_within = c(1,1,4),
#             test_type = "one-tailed")

})
