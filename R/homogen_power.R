#' Compute Power for Test of Homogeneity in Meta-analysis
#'
#' Compute statistical power for the Test of Homogeneity for meta-analysis under both fixed- and random-effects models.
#'
#' @param effect_size  Numerical value of effect size.
#'
#' @param study_size Numerical value for number number of participants (per study).
#'
#' @param k Numerical value for total number of studies.
#'
#' @param i2 Numerical value for Heterogeneity estimate (i^2).
#'
#' @param es_type 'Character reflecting effect size metric: 'r', 'd', or 'or'.
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
#' @return Estimated Power to detect differences in homogeneity of effect sizes for fixed- and random-effects models
#'
#' @examples
#' homogen_power(effect_size = .5, study_size = 10, k = 10, i2 = .50, es_type = "d")
#'
#' @seealso
#' \url{https://jason-griffin.shinyapps.io/shiny_metapower/}
#'
#' @references
#'
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T. and Rothstein, H. R.(2009). Introduction to meta-analysis, Chichester, UK: Wiley.
#'
#' Hedges, L., Pigott, T. (2004). The Power of Statistical Tests for Moderators in Meta-Analysis, Psychological Methods, 9(4), 426-445.
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
#' @importFrom stats dchisq
#' @importFrom stats integrate
#' @importFrom stats pgamma
#' @import ggplot2
#' @import magrittr
#' @export

homogen_power <- function (effect_size, study_size, k, i2, es_type, p =.05, con_table = NULL){

  if(missing(effect_size))
    effect_size = NULL
  ## check args
  homogen_power_integrity(effect_size, study_size, k, i2, es_type, p, con_table)

  df <- k-1 # between-groups df
  c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE) # critical value chi-square dist
  range_factor <- 5

  if(es_type == "d"){

    variance <- compute_variance(study_size, effect_size, es_type, con_table)
    # create a power range of data
    homogen_power_range_df <- data.frame(k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = study_size,
                                         i2 = i2,
                                         c_alpha = c_alpha) %>%
      mutate(variance = mapply(compute_variance, .data$n_v, .data$es_v, es_type))

  }else if(es_type == "r"){
    ## Convert to fishers-z
    effect_size = .5*log((1 + effect_size)/(1 - effect_size))
    variance <- compute_variance(study_size, effect_size, es_type, con_table)
    homogen_power_range_df <- data.frame(sd_v = rep(seq(0,6), each = (k*range_factor-1)),
                                         k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = study_size,
                                         i2 = i2,
                                         c_alpha = c_alpha) %>% mutate(variance = mapply(compute_variance, .data$n_v, .data$es_v, es_type))


  }else if(es_type == "or") {
    ## Convert odd ratio to log of odds ratio: log(OR)
    effect_size <- round((con_table[1]*con_table[4])/(con_table[2]*con_table[3]),3)
    effect_size <- round(log(effect_size),3)

    ## Compute common variance
    variance <- round((1/con_table[1]) + (1/con_table[2]) + (1/con_table[3]) + (1/con_table[4]),3)
    homogen_power_range_df <- data.frame(sd_v = rep(seq(0,6), each = (k*range_factor-1)),
                                         k_v = rep(seq(2,range_factor*k),times = 7),
                                         es_v = effect_size,
                                         n_v = study_size,
                                         i2 = i2,
                                         c_alpha = c_alpha,
                                         variance = variance)
  }

  # Generate list of relevant variables for output
  power_list <- list(variance = variance,
                     homogen_power_range_df = homogen_power_range_df,
                     homogen_power = compute_homogen_power(k, effect_size, variance, i2, c_alpha),
                     homogen_power_range = compute_homogen_range(homogen_power_range_df),
                     effect_size = effect_size,
                     study_size = study_size,
                     i2 = i2,
                     k = k,
                     es_type = es_type,
                     p = p)
  attr(power_list, "class") <- "homogen_power"

  return(power_list)
}
