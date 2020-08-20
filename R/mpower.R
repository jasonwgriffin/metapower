#' Compute Power for Meta-analysis
#'
#' Computes statistical power for meta-analytic main effects, tests of homogeneity, and categorical moderator models under
#' both fixed- and random-effects models.
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
#' @param sd (Optional) Fixed-effects models only: Expected standard deviation among all effect sizes
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

mpower <- function(effect_size, sample_size, k, es_type, test_type = "two-tailed", p = .05, sd = NULL, con_table = NULL){

  ## Check that the arguments are correctly specified
  mpower_integrity(effect_size, sample_size, k, es_type, test_type, p, sd, con_table)

  ## Transform effect sizes condition on the metric

  effect_size = abs(effect_size)

  if(es_type == "Correlation"){

    effect_size = .5*log((1 + effect_size)/(1 - effect_size))

    }else if(es_type == "OR") {

      effect_size = log(effect_size)

      }

# Compute common variance
variance <- compute_variance(sample_size, effect_size, es_type, con_table)
# Generate list of relevant variables for output
power_list <- list(variance = variance,
                   power = compute_power(effect_size, variance, sample_size, k, es_type, test_type, p),
                   effect_size = effect_size,
                   sample_size = sample_size,
                   k = k,
                   es_type = es_type,
                   test_type = test_type,
                   p = p,
                   sd = sd,
                   df = compute_power_range(effect_size, sample_size, k, es_type, test_type, p, con_table),
                   homo_power = homogen_power(effect_size, variance, sample_size, k, es_type, test_type, p, sd),
                   homo_range = compute_homogen_range(effect_size, sample_size, k, es_type, test_type, p, sd, con_table))
attr(power_list, "class") <- "mpower"

return(power_list)
}
