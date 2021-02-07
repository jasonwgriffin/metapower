#' Compute Power for Meta-analysis
#'
#' Computes statistical power for summary effect sizes in meta-analysis.
#'
#' @param effect_size  Numerical value of effect size.
#'
#' @param study_size Numerical value for number number of participants (per study).
#'
#' @param k Numerical value for total number of studies.
#'
#' @param i2 Numerical value for Heterogeneity estimate (i^2).
#'
#' @param es_type Character reflecting effect size metric: 'r', 'd', or 'or'.
#'
#' @param test_type Character value reflecting test type: ("two-tailed" or "one-tailed").
#'
#' @param p Numerical value for significance level (Type I error probability).
#'
#' @param con_table (Optional) Numerical values for 2x2 contingency table as a vector in the following format: c(a,b,c,d).
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
#' mpower(effect_size = .2, study_size = 10, k = 10, i2 = .5, es_type = "d")
#'
#' @seealso
#' \url{https://jason-griffin.shinyapps.io/shiny_metapower/}
#'
#' @references
#'
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T. and Rothstein, H. R.(2009). Introduction to meta-analysis, Chichester, UK: Wiley.
#'
#' Hedges, L., Pigott, T. (2004). The Power of Statistical Tests for Moderators in Meta-Analysis, Psychological Methods, 9(4), 426-445
#' doi: https://dx.doi.org/10.1037/1082-989x.9.4.426
#'
#' Pigott, T. (2012). Advances in Meta-Analysis.
#' doi: https://dx.doi.org/10.1007/978-1-4614-2278-5
#'
#' Jackson, D., Turner, R. (2017). Power analysis for random-effects meta-analysis, Research Synthesis Methods, 8(3), 290-302
#' doi: https://dx.doi.org/10.1002/jrsm.1240
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @importFrom stats dchisq
#' @importFrom stats integrate
#' @importFrom stats pgamma
#' @importFrom testthat test_that
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_error
#' @importFrom testthat expect_match
#' @importFrom rlang .data
#' @importFrom knitr knit
#' @import ggplot2
#' @import magrittr
#' @export

mpower <- function(effect_size, study_size, k, i2, es_type, test_type = "two-tailed", p = .05, con_table = NULL){

  if(missing(effect_size))
    effect_size = NULL
  ## Check that the arguments are correctly specified
  mpower_integrity(effect_size, study_size, k, i2, es_type, test_type, p, con_table)

  ## Transform effect sizes condition on the metric
  ## Determine the critical value cut-of based on one or two tailed test and p-value
  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }
  ## range of studies for visualization
  range_factor <- 5

  if(es_type == "d"){

    variance <- compute_variance(study_size, effect_size, es_type, con_table)
    # create a power range of data
    power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                                 es_v = effect_size,
                                 n_v = study_size,
                                 i2 = i2,
                                 c_alpha = c_alpha,
                                 test_type = test_type) %>% mutate(variance = mapply(compute_variance, .data$n_v, .data$es_v, es_type))

    } else if (es_type == "r"){
    ## Convert to fishers-z
    effect_size = round(.5*log((1 + effect_size)/(1 - effect_size)),2)
    ## Compute common variance
    variance <- compute_variance(study_size, effect_size, es_type, con_table)
    ## Create power range of data
    ### Restrict range based on limitations. For example, a correlation of 1 = fisher's z of 2.64
    if(effect_size*2 >= 2.64){
      max = 2.64
      } else {
        max = effect_size*2
        }
    ## Create power range of data
    power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                                 es_v = effect_size,
                                 n_v = study_size,
                                 i2 = i2,
                                 c_alpha = c_alpha,
                                 test_type = test_type) %>% mutate(variance = mapply(compute_variance, .data$n_v, .data$es_v, es_type))

    }else if(es_type == "or") {
      ## Convert odd ratio to log of odds ratio: log(OR)

      effect_size <- round((con_table[1]*con_table[4])/(con_table[2]*con_table[3]),3)
      effect_size <- round(log(effect_size),3)

      ## Compute common variance
      variance <- round((1/con_table[1]) + (1/con_table[2]) + (1/con_table[3]) + (1/con_table[4]),3)

      # Create power range of data
      power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k), times = 3),
                                   es_v = effect_size,
                                   n_v = study_size,
                                   i2 = i2,
                                   c_alpha = c_alpha,
                                   test_type = test_type,
                                   variance = variance)
      }
# Generate list of relevant variables for output
  power_list <- list(variance = variance,
                   power = compute_power(k, effect_size, variance, i2, c_alpha, test_type),
                   power_range = compute_power_range(power_range_df),
                   effect_size = effect_size,
                   study_size = study_size,
                   i2 = i2,
                   k = k,
                   es_type = es_type,
                   test_type = test_type,
                   p = p)
  attr(power_list, "class") <- "mpower"

  return(power_list)
}
