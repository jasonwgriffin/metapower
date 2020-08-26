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
                      effect_sizes,
                      sample_size,
                      k,
                      es_type,
                      test_type = "two-tailed",
                      p = .05,
                      sd_within = NULL,
                      con_table = NULL) {

  ## Argument Check
  mod_power_integrity(effect_sizes, sample_size, k, es_type, test_type, p, con_table, sd_within)

  if(test_type == "two-tailed"){
    c_alpha_b <- qchisq(1-(p/2), df_b, 0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-(p/2), df_w, 0, lower.tail = TRUE)

  }else if(test_type == "one-tailed"){
    c_alpha_b <- qchisq(1-p, df_b, 0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-p, df_w, 0, lower.tail = TRUE)
  }

  #####
  #####
  ##### common variance?>
  variance <- compute_variance(sample_size, overall_effect_diff, es_type, con_table)


  ## Difference in effect sizes
  effect_diff <- effect_sizes - effect_sizes[1]

  ##
  overall_effect <- mean(effect_sizes) # find overall mean

  if(es_type == "Correlation"){
    effect_sizes <- 0.5*log((1+effect_diff)/(1-effect_diff)) ## changes correlation to fisher's z
  }else if(es_type == "OR") {
    effect_sizes = log(effect_sizes) ## changes odds ratio to log odds
  }

  mod_power_list <- list(mod_power = compute_mod_power(n_groups, effect_sizes, sample_size, k, es_type, test_type, p, sd_within, con_table),
                         n_groups = n_groups,
                         effect_sizes = effect_sizes,
                         sample_size = sample_size,
                         k = k,
                         es_type = es_type,
                         sd_within = sd_within,
                         con_table = con_table)
  attr(mod_power_list, "class") <- "mod_power"
  return(mod_power_list)
}
