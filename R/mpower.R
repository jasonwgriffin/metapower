#' Compute Power for Meta-analysis
#'
#' the mpower( ) command allows for the calculation of statistical power for both fixed- and random-effects
#' meta-analyis models. Currently, the anticpated summary effect size estimate should be standardized mean differences units
#'(i.e, Cohen's d)
#'
#' @param effect_size  anticipated magnitude of effect size.
#'
#' @param sd expected difference in effect sizes
#'
#' @param sample_size anticipated average number of participants per group
#'
#' @param k anticipated number of studies
#'
#' @param es_type Effect size metric: 'Correlation', 'd', or 'OR'
#'
#' @param model Fixed-effects model (model = "fixed") or Random-effects model (model = "random")
#'
#' @param hg anticipated heterogenity estimate (smalll = 1.33, moderate = 1.67, large = 2)
#'
#' @param test_type one-tailed or two-tailed
#'
#' @param p alpha level: p = .05 (DEFAULT)
#'
#' @return Estimated Power
#'
#' @examples
#' mpower(effect_size = .5, sample_size = 10, k = 10, hg = "large", es_type = "d",
#'        model = "random", test_type = "two-tailed", sd = .5)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @import ggplot2
#' @import tibble
#' @import magrittr
#' @export

mpower <- function(effect_size, sample_size, k, es_type, model, hg, test_type = "two-tailed", p = .05, sd = NULL){

  model_options <- c("fixed", "random")
  test_type_options <- c("one-tailed", "two-tailed")
  hg_options <- c("small", "medium", "large")
  es_type_options <- c("d","Correlation", "OR")
  hg_options <- c("small", "medium", "large")

  ## Effect size integrity checks
  if(missing(effect_size))
    stop("Need to specify expected effect size")
  if(!(is.numeric(effect_size)))
    stop("effect_size must be numeric")
  if(length(effect_size) > 1)
    stop("effect_size must be a single number")
  if(effect_size > 10)
    warning("Are you sure effect size is >10?")

  # ## sd integrity checks
  # if(missing(sd))
  #   stop("Need to specify expected standard deviation of effect sizes")
  # if(!(is.numeric(sd)))
  #   stop("sd must be numeric")
  # if(length(sd) > 1)
  #   stop("sd must be a single number")
  # if(sd > 10)
  #   warning("Are you sure standard deviation is >10?")

  ## sample_size integrity checks
  if(missing(sample_size))
    stop("Need to specify expected sample size")
  if(!(is.numeric(sample_size)))
    stop("sample_size must be numeric")
  if(length(sample_size) > 1)
    stop("sample_size must be a single number")
  if(sample_size < 1)
    stop("sample_size must be greater than 0")

  ## num studies integrity checks
  if(missing(k))
    stop("Need to specify expected number of studies")
  if(!(is.numeric(k)))
    stop("k must be numeric")
  if(length(k) > 1)
    stop("k must be a single number")
  if(k < 2)
    stop("k must be greater than 1")

  ## es_type
  if(missing(es_type))
    stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")
  if(!(es_type %in% es_type_options))
    stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")

  ## test_type errors
  if(!(test_type %in% test_type_options))
    stop("Need to specify two-tailed or one-tailed")

  # model
  if(missing(model))
    stop("Need to specify 'fixed' or 'random' effects model")
  if(!(model %in% model_options))
    stop("Need to specify 'fixed' or 'random' effects model")

  ## random effects and heterogenity parameter
  if(model == 'random' & missing(hg))
    stop("Need to specify small, medium, or large heterogenity")
  if(model == "random" & !(hg %in% hg_options))
    stop("Need to specify small, medium, or large heterogenity")

  effect_size = abs(effect_size)

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
                     es_type = es_type,
                     hg = hg,
                     model = model,
                     test_type = test_type,
                     p = p,
                     df = compute_power_range(effect_size, sample_size, k, model, test_type, p))

  if(!(missing(sd))){
    power_list <- append(power_list, list(sd = sd, homo_test = homogen_mpower(effect_size, sample_size, k, hg, model, test_type, p, sd)))
  } else {
    power_list <- append(power_list, list(sd = NA, homo_test = NA))
  }

  attr(power_list, "class") <- "mpower"
  return(power_list)
}
