#' Compute Power for Test of Homogeneity in Meta-analysis
#'
#' Computes statistical power for the Test of Homogeneity for meta-analytic under both fixed- and random-effects models.
#'
#' @param effect_size  Expected effect size magnitude
#'
#' @param sample_size Expected number of participants (per group)
#'
#' @param k Expected number of studies
#'
#' @param es_type 'Correlation', 'd', or 'OR'
#'
#' @param test_type "two-tailed" or "one-tailed"
#'
#' @param i2 Heterogeneity parameter (I^2 statistic)
#'
#' @param p Significance level (Type I error probability)
#'
#' @param con_table (Optional) For Odds Ratio. Expected 2x2 contingency table as a vector in the following format: c(a,b,c,d)
#'
#' \tabular{lcc}{
#'  2x2 Table   \tab Group 1 \tab Group 2 \cr
#'  Present     \tab a       \tab b       \cr
#'  Not Present \tab c       \tab d       \cr
#'}
#'
#' @return Estimated Power with across various levels of heterogeneity
#'
#' @examples
#' homogen_power(effect_size = .5, sample_size = 10, k = 10, es_type = "d")
#'
#' @references
#'
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T. and Rothstein, H. R.(2009). Introduction to meta-analysis, Chichester, UK: Wiley.
#'
#' Hedges, L., Pigott, T. (2004). The Power of Statistical Tests for Moderators in Meta-Analysis Psychological Methods  9(4), 426-445.
#' doi: https://dx.doi.org/10.1037/1082-989x.9.4.426
#'
#' Pigott, T. (2012). Advances in Meta-Analysis.
#' doi: https://dx.doi.org/10.1007/978-1-4614-2278-5
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @import ggplot2
#' @import magrittr
#' @export

homogen_power <- function (effect_size, sample_size, k, es_type, test_type = "two-tailed", p =.05, i2 = .50, con_table = NULL){

  df <- k-1

  if(test_type == "two-tailed"){
    c_alpha <- qchisq(1-(p/2),df,0, lower.tail = TRUE)
  } else if (test_type =="one-tailed") {
    c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
  }

  range_factor <- 5

  if(es_type == "d"){

    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    # create a power range of data
    homogen_power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = sample_size,
                                         c_alpha = c_alpha) %>% mutate(variance = mapply(compute_variance, n_v, es_v, es_type))

  }else if(es_type == "Correlation"){
    ## Convert to fishers-z
    effect_size = .5*log((1 + effect_size)/(1 - effect_size))
    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    homogen_power_range_df <- data.frame(sd_v = rep(seq(0,6), each = (k*range_factor-1)),
                                         k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = sample_size,
                                         c_alpha = c_alpha) %>% mutate(variance = mapply(compute_variance, n_v, es_v, es_type))


  }else if(es_type == "OR") {
    ## Convert odd ratio to log of odds ratio: log(OR)
    effect_size = log(effect_size)
    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    homogen_power_range_df <- data.frame(sd_v = rep(seq(0,6), each = (k*range_factor-1)),
                                         k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = sample_size,
                                         c_alpha = c_alpha,
                                         variance = variance)
  }

  # Generate list of relevant variables for output
  power_list <- list(variance = variance,
                     homogen_power_range_df = homogen_power_range_df,
                     homogen_power = compute_homogen_power(k, effect_size, variance, c_alpha),
                     homogen_power_range = compute_homogen_range(homogen_power_range_df),
                     effect_size = effect_size,
                     sample_size = sample_size,
                     k = k,
                     es_type = es_type,
                     test_type = test_type,
                     p = p,
                     i2 = i2)
  attr(power_list, "class") <- "homogen_power"

  return(power_list)
}
