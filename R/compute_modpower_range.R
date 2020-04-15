
compute_modpower_range <- function(n_groups, effect_sizes, overall_effect_diff, sample_size, k, es_type, test_type, p, sd_within, con_table){

  if(missing(con_table))
    con_table <- NULL
  if(missing(sd_within))
    sd_within <- NULL
  #show 5x as many studies as user enters
  range_factor <- 5

  df <- data.frame(k_v = rep(seq(k,range_factor*k), times = 1))
  variance <- compute_variance(sample_size, overall_effect_diff, es_type, con_table)
  modpower_range <- cbind.data.frame(df, as.data.frame((t(mapply(compute_mod_power, n_groups, variance, effect_sizes, overall_effect_diff, sample_size, df$k_v, es_type, test_type, p)))) %>%
                                       dplyr::mutate_all(as.numeric))

  return(modpower_range)
}

# n_groups <- 2
# effect_sizes <- c(.1,.5)
# sample_size <- 10
# k <- 10
# range_factor <- 5
# df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 1))
# k_v = rep(seq(2,range_factor*k), times = 1)
# es_type = "d"
# sd_within = NULL
# con_table = NULL
# test_type = "two-tailed"
# p = .05
# variance = .22
# overall_effect_diff = .11
# t(mapply(compute_mod_power,
#        n_groups,
#        variance,
#        effect_sizes,
#        overall_effect_diff,
#        sample_size,
#        k_v,
#        es_type,
#        test_type,
#        p))
#        #sd_within,
#        #con_table)
#
# c_alpha_w <- qchisq(1-(p/2), 1,0,lower.tail = TRUE)
