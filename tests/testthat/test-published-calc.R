context("Check values match those from which the formulas were derived")

test_that("Check power test from Pigott 2012", {

  ## Section 4.3.4 - Page 43
  expect_equal(round(mpower(effect_size = .5,
                            sample_size = 40,
                            k = 10,
                            es_type ="d",
                            test_type = "one-tailed")$power$fixed_power,2), 1)

  ## Section 4.3.4 - Page 44
  expect_equal(round(mpower(effect_size = .2,
                            sample_size = 20,
                            k = 10,
                            es_type ="d")$power$fixed_power,2), .29)
  # Section 4.3.5 - Page 46
  expect_equal(round(mpower(sample_size = 2000,
                            k = 9,
                            es_type = "or",
                            test_type = "one-tailed",
                            con_table = c(26,974,20,980))$power$fixed_power,2), .85)

  ## Random Effects Validation


  ## Section 17.2.3 - Page 353
  expect_equal(round(subgroup_power(n_groups = 2,
                               effect_sizes = c(0,.5),
                               sample_size = 40,
                               k = 10,
                               es_type = "d")$subgroup_power$fixed_power_b,2), .70)

  ## Section 17.2.7 - Page 358
  expect_equal(round(subgroup_power(n_groups = 2,
                                    effect_sizes = c(0,.5),
                                    sample_size = 40,
                                    k = 10,
                                    es_type = "d")$subgroup_power$random_power_b_l,2), .24)

  ## Pigott 2012 - Page 71 Section 6.2.4
  expect_equal(round(mod_power(n_groups = 3,
                 effect_sizes = c(0,.1,.5),
                 sample_size = 15,
                 k = 15,
                 es_type = "r")$mod_power$fixed_power_b,2),.83)

})

