compute_mod_range <- function(n_groups, effect_sizes, mod_power_range_df){

  v <- rep(list(effect_sizes),times = 1)
  ####
  mod_power_range_df <- cbind.data.frame(mod_power_range_df, as.data.frame((t(mapply(compute_mod_power,
                                                                                      mod_power_range_df$n_groups,
                                                                                     v,
                                                                                      mod_power_range_df$variance,
                                                                                      mod_power_range_df$overall_effect,
                                                                                      mod_power_range_df$n_v,
                                                                                      mod_power_range_df$k_v,
                                                                                      mod_power_range_df$c_alpha_b)))) %>%
                                               dplyr::mutate_all(as.numeric))



  return(mod_power_range_df)
}
