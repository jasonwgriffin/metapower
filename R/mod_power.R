#' Compute Power for Categorical Moderation Meta-analysis
#'
#' Computes statistical power for categorical moderator models under fixed- and random-effects models.
#'
#' @param n_groups Number of anticipated groups in moderation analysis
#'
#' @param effect_sizes  Expected effect sizes of for each group.
#'
#' @param es_type 'Correlation', 'd', or 'OR'
#'
#' @param sample_size Expected number of participants (per group)
#'
#' @param k Total expected number of studies
#'
#' @param p Significance level (Type I error probability)
#'
#' @param test_type "two-tailed" or "one-tailed"
#'
#' @param sd_within (Optional) For computing power for a test of homogeneity (within-groups). standard deviation of each group to the overall mean
#'
#' @param con_table (Optional) For Odds Ratio effect sizes. Expected 2x2 contingency table as a vector in the following format: c(a,b,c,d)
#'
#' \tabular{lcc}{
#'  2x2 Table   \tab Group 1 \tab Group 2 \cr
#'  Present     \tab a       \tab b       \cr
#'  Not Present \tab c       \tab d       \cr
#'}
#'
#' @return Estimated Power estimates for between and within-groups moderation
#'
#' @examples
#' mod_power(
#'  n_groups = 3,
#'  effect_sizes = c(0,.1,.55),
#'  sample_size = 15,
#'  k = 15,
#'  es_type = "Correlation",
#'  sd_within = c(1,1,4),
#'  test_type = "two-tailed",
#'  p = .05)
#'
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @importFrom stats dchisq
#' @importFrom stats integrate
#' @importFrom stats pgamma
#' @export

mod_power <- function(n_groups,
                      effect_sizes = NULL,
                      sample_size,
                      k,
                      es_type,
                      test_type = "two-tailed",
                      p = .05,
                      con_table = NULL) {
                      #sd_within = NULL) {

  ## Argument Check
  mod_power_integrity(n_groups, effect_sizes, sample_size, k, es_type, test_type, p, con_table) #s,d_within)

  df_b <- n_groups-1
  df_w <- k-n_groups

  if(test_type == "two-tailed"){
    c_alpha_b <- qchisq(1-(p/2), df_b, 0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-(p/2), df_w, 0, lower.tail = TRUE)

  }else if(test_type == "one-tailed"){
    c_alpha_b <- qchisq(1-p, df_b, 0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-p, df_w, 0, lower.tail = TRUE)
  }

  #####
  #####

  ## assume equal groups across conditions

  #sample_size <- sample_size/2

  range_factor <- 5

  ##### common variance?>
  #variance <- compute_variance(sample_size, overall_effect, es_type, con_table)

  if(es_type == "d"){
    effect_diff <- effect_sizes - effect_sizes[1] # difference in effects
    overall_effect <- mean(effect_sizes) # find overall mean
    variance <- compute_variance(sample_size/2, overall_effect, es_type, con_table)

    # create a power range of data

    mod_power_range_df <- data.frame(k_v = seq(from = n_groups, to = range_factor*k, by = n_groups),
                                 #es_v = rep(c((effect_sizes/2), effect_size, (effect_size*2)), each = range_factor*k-1),
                                 overall_effect = overall_effect,
                                 n_groups = n_groups,
                                 n_v = sample_size,
                                 c_alpha_b = c_alpha_b,
                                 c_alpha_w = c_alpha_w,
                                 es_1 = 0,
                                 es_2 = .05) %>% dplyr::mutate(variance = mapply(compute_variance, .data$n_v, .data$overall_effect, es_type))

    }else if(es_type == "Correlation"){
      effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes)) ## changes correlation to fisher's z
      effect_diff <- effect_sizes - effect_sizes[1] # difference in effects
      overall_effect <- mean(effect_sizes) # find overall mean
      variance <- compute_variance(sample_size/2, overall_effect, es_type, con_table)

      }else if(es_type == "OR") {



        group <- names(con_table)
        d <- data.frame(group)
        d$a <- sapply(con_table, "[[", 1)
        d$b <- sapply(con_table, "[[", 2)
        d$c <- sapply(con_table, "[[", 3)
        d$d <- sapply(con_table, "[[", 4)
        d$or <- round((d$a*d$d)/(d$b*d$c),3)
        d$log_or <- round(log(d$or),3)
        d$var <- round((1/d$a) + (1/d$b) + (1/d$c) + (1/d$d),3)
        effect_diff <- d$log_or - d$log_or[1]
        overall_effect <- mean(d$log_or) # find overall mean
        variance <- mean(d$var) # .82 pigott
        effect_sizes <- d$log_or
  }

  mod_power_list <- list(mod_power = compute_mod_power(n_groups, effect_sizes, variance, overall_effect, sample_size, k, c_alpha_b),
                         mod_power_range = compute_mod_range(n_groups, effect_sizes, mod_power_range_df),
                         mod_power_range_df = mod_power_range_df,
                         n_groups = n_groups,
                         effect_sizes = effect_sizes,
                         sample_size = sample_size,
                         k = k,
                         es_type = es_type,
                         #sd_within = sd_within,
                         #df = d,
                         variance = variance)
  attr(mod_power_list, "class") <- "mod_power"
  return(mod_power_list)
}
