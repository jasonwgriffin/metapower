#' Compute Power for Meta-analysis
#'
#' the mpower( ) command allows for the calculation of statistical power for both fixed- and random-effects
#' meta-analyis models. Currently, the anticpated summary effect size estimate should be standardized mean differences units
#'(i.e, Cohen's d)
#'
#' @param effect_size  anticipated magnitude of effect size.
#'
#' @param sample_size anticipated average number of participants per group
#'
#' @param k anticipated number of studies
#'
#' @param hg anticipated heterogenity estimate (smalll = 1.33, moderate = 1.67, large = 2)
#'
#' @param model Fixed-effects model (model = "fixed") or Random-effects model (model = "random")
#'
#' @param p alpha level: p = .05 (DEFAULT)
#'
#' @param test_type one-tailed or two-tailed
#'
#' @return Estimated power
#'
#' @examples
#'
#' mpower(effect_size = .5, sample_size = 25, k = 10, hg = 1.67, model = c("fixed", "random"), test_type = c("one=tailed, "two-tailed"))
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @import ggplot2
#' @import tibble
#' @import magrittr
#' @export

mpower <- function(effect_size,
                   sample_size,
                   k,
                   hg = c("small", "medium", "large"),
                   es_type = c("Correlation", "d", "OR"),
                   model = c("fixed", "random"),
                   test_type = c("one-tailed", "two-tailed"),
                   p = .05,
                   sd){

  model_options <- c("fixed", "random")
  test_type_options <- c("one-tailed", "two-tailed")

  if(missing(effect_size))
    stop("Need to specify anticipated effect size")
  if(effect_size < 0)
    stop("Specifiy effect size in positive units")
  if(missing(sample_size))
    stop("Need to specify anticipated sample size")
  if(sample_size < 1)
    stop("Sample size must be greater than 0")
  if(missing(k))
    stop("Need to specify anticipated number of studies")
  if(k < 2)
    stop("Number of studies must be greater than 1")
  if(missing(model))
    stop("Need to specify type of model")
  if(!(model %in% model_options))
    stop("Need to specify 'fixed' or 'random' effects model")
  if(!(test_type %in% test_type_options))
    stop("Need to specify one-tailed or two-tailed")


  if(es_type == "Correlation"){
    effect_size = 2*effect_size/sqrt(1-effect_size^2)
    es_type = "d"
  }else if(es_type == "OR") {
    effect_size = effect_size*(sqrt(3)/pi)
    es_type = "d"
  }

  power_list <- list(power = compute_power(effect_size, sample_size, k, hg, model, test_type, p, es_type),
                     effect_size = effect_size,
                     sample_size = sample_size,
                     k = k,
                     hg = hg,
                     model = model,
                     test_type = test_type,
                     p = p,
                     sd =sd,
                     df = compute_power_range(effect_size, sample_size, k, model, test_type, p),
                     homo_test = homogen_mpower(effect_size, sample_size, k, hg, model, test_type, p, sd))
  attr(power_list, "class") <- "mpower"
  return(power_list)
}


