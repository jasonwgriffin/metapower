compute_subgroup_range <- function(n_groups, effect_sizes, subgroup_power_range_df){

  v <- rep(list(effect_sizes),times = 1)
  ####
  subgroup_power_range_df <- cbind.data.frame(subgroup_power_range_df,
                                              as.data.frame((t(mapply(compute_subgroup_power,
                                                                      subgroup_power_range_df$n_groups,
                                                                      v,
                                                                      subgroup_power_range_df$variance,
                                                                      subgroup_power_range_df$i2,
                                                                      subgroup_power_range_df$overall_effect,
                                                                      subgroup_power_range_df$n_v,
                                                                      subgroup_power_range_df$k_v,
                                                                      subgroup_power_range_df$c_alpha_b)))) %>% dplyr::mutate_all(as.numeric))

  return(subgroup_power_range_df)
}
