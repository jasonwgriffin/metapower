compute_power_range <- function(power_range_df){

  power_range_df <- cbind.data.frame(power_range_df, as.data.frame((t(mapply(compute_power,power_range_df$k_v,power_range_df$es_v, power_range_df$variance, power_range_df$c_alpha,power_range_df$test_type )))) %>%
                           dplyr::mutate_all(as.numeric))

  return(power_range_df)

}
