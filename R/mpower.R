#' Compute Power for Meta-analysis
#'
#' Computes statistical power for meta-analytic under both fixed- and random-effects models.
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
#' @return Estimated Power
#'
#' @examples
#' mpower(effect_size = .5, sample_size = 10, k = 10, es_type = "d")
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

mpower <- function(effect_size, sample_size, k, es_type, test_type = "two-tailed", p = .05, i2 = .50, con_table = NULL){

  ## Check that the arguments are correctly specified
  mpower_integrity(effect_size, sample_size, k, es_type, test_type, p, con_table)

  ## Transform effect sizes condition on the metric

  effect_size = abs(effect_size)

  ## Determine the critical value cut-of based on one or two tailed test and p-value
  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }

  range_factor <- 5

  if(es_type == "d"){

    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    # create a power range of data
    power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                     es_v = rep(c((effect_size/2), effect_size, (effect_size*2)), each = range_factor*k-1),
                     effect_size = effect_size,
                     i2_v = i2,
                     n_v = sample_size,
                     c_alpha = c_alpha) %>% mutate(variance = mapply(compute_variance, n_v, es_v, es_type))

    } else if (es_type == "Correlation"){
    ## Convert to fishers-z
    effect_size = .5*log((1 + effect_size)/(1 - effect_size))
    ## Compute common variance
    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    ## Create power range of data
    ### Restrict range based on limitations. For example, a correlation of 1 = fisher's z of 2.64
    if(effect_size*2 >= 2.64){
      max = 2.64
      } else {
        max = effect_size*2
        }
    ## Create power range of data
    power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                                 es_v = rep(c((effect_size/2), effect_size, max), each = range_factor*k-1), ## special for correlation
                                 effect_size = effect_size,
                                 i2_v = i2,
                                 n_v = sample_size,
                                 c_alpha = c_alpha) %>% mutate(variance = mapply(compute_variance, n_v, es_v, es_type))

    }else if(es_type == "OR") {
      ## Convert odd ratio to log of odds ratio: log(OR)
      effect_size = log(effect_size)
      ## Compute common variance
      variance <- compute_variance(sample_size, effect_size, es_type, con_table)

      # Create power range of data
      power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                                   es_v = rep(c((effect_size/2), effect_size, (effect_size*2)), each = range_factor*k-1),
                                   effect_size = effect_size,
                                   i2_v = i2,
                                   n_v = sample_size,
                                   c_alpha = c_alpha,
                                   variance = variance)
      }
# Generate list of relevant variables for output
  power_list <- list(variance = variance,
                   power = jackson_power(k, effect_size, variance, i2, c_alpha),
                   power_range = compute_jackson_power_range(power_range_df),
                   effect_size = effect_size,
                   sample_size = sample_size,
                   k = k,
                   es_type = es_type,
                   test_type = test_type,
                   p = p,
                   i2 = i2)
  attr(power_list, "class") <- "mpower"

  return(power_list)
}
