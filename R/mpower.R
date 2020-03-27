#' Compute Power for Meta-analysis
#'
#' the mpower( ) command allows for the calculation of statistical power for both fixed- and random-effects
#' meta-analyis models. Currently, the anticpated summary effect size estimate should be standardized mean differences units
#'(i.e, Cohen's d)
#'
#' @param effect_size  Expected effect size magnitude.
#'
#' @param sample_size Expected number of participants (per group).
#'
#' @param k Expected number of studies.
#'
#' @param es_type Effect size metric: 'Correlation', 'd', or 'OR'
#'
#' @param model Fixed-effects model (model = "fixed") or Random-effects model (model = "random")
#'
#' @param hg Expected heterogenity: "small", "medium", or "large".
#'
#' @param test_type Alternative hypothesis: "two-tailed" (default) or "one-tailed"
#'
#' @param p alpha level: p = .05 (default)
#'
#' @param sd (Optional) Fixed-effects models only: Expected standard deviation of among all effect sizes
#'
#' @param con_table Only relevant for Odds Ratio. Expected 2x2 contingency table as a vector in the following format: c(a,b,c,d)
#'
#' \tabular{lcc}{
#'  2x2 Table   \tab Group 1 \tab Group 2 \cr
#'  Present     \tab a       \tab b       \cr
#'  Not Present \tab c       \tab d       \cr
#'}
#'
#'
#' @return Estimated Power
#'
#' @examples
#' mpower(effect_size = .5, sample_size = 10, k = 10, es_type = "d",
#'        hg = "large", model = "random", test_type = "two-tailed")
#'
#' @references
#'
#' Bornstein, M., Hedges, L. V., Higgins, J. P. T. and Rothstein, H. R.(2009). Introduction to meta-analysis, Chichester, UK: Wiley.
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
#' @import tibble
#' @import magrittr
#' @export

mpower <- function(effect_size, sample_size, k, es_type, model, hg, test_type = "two-tailed", p = .05, sd, con_table){

  model_options <- c("fixed", "random")
  test_type_options <- c("one-tailed", "two-tailed")
  hg_options <- c("small", "medium", "large")
  es_type_options <- c("d","Correlation", "OR")


  ## Effect size integrity checks
  if(missing(effect_size))
    stop("Need to specify expected effect size")
  if(!(is.numeric(effect_size)))
    stop("effect_size must be numeric")
  if(length(effect_size) > 1)
    stop("effect_size must be a single number")
  if(effect_size < 0)
    stop("effect_size must be greater than zero")

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
  ## effect size
  if(es_type == 'd' & effect_size > 10)
    warning("Are you sure effect size is >10?")
  if(es_type == 'Correlation' & effect_size > 1)
    stop("Correlation cannot be above 1")
  if(es_type == 'Correlation' & effect_size < 0)
    stop("Correlation must be above 0")

  if(es_type == 'OR' & effect_size <= 1)
    stop("Odds ratio should be above 1")
  if(es_type == "OR" & missing(con_table))
    stop("For Odds Ratio, must enter contigency table (cont_table)")
  if(es_type == "OR" & !missing(con_table)){
      if(length(con_table) != 4)
        stop("con_table must reflect a 2x2 contingency table with the form c(a,b,c,d). see documentation")
      if(sample_size != sum(con_table))
        stop("Entered sample size should equal the sum of the contigency table")
    }


  ## test_type errors
  if(!(test_type %in% test_type_options))
    stop("Need to specify two-tailed or one-tailed")

  # model
  if(missing(model))
    stop("Need to specify 'fixed' or 'random' effects model")
  if(!(model %in% model_options))
    stop("Need to specify 'fixed' or 'random' effects model")

  ## check for arguements that are not required for fixed models
  if(model == "fixed"){
    if (!missing(hg)){
      stop("Fixed-effects models assume no heterogenity")
    } else if (missing(hg)) {
      hg = NULL
    }
  }

  ## random effects and heterogenity parameter
  if(model == 'random'){
    if(missing(hg)){
      stop("Need to specify small, medium, or large heterogenity")
    } else if (!(hg %in% hg_options)){
      stop("Need to specify small, medium, or large heterogenity")
    }
  }

  effect_size = abs(effect_size)

  if(es_type == "Correlation"){

    effect_size = .5*log((1 + effect_size)/(1 - effect_size))

    }else if(es_type == "OR") {

      effect_size = log(effect_size)

      }

# Compute common variance
variance <- compute_variance(sample_size, effect_size, es_type, con_table)

  if(missing(sd) & model == "fixed"){
    power_list <- list(variance = variance,
                       power = compute_power(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p),
                       effect_size = effect_size,
                       sample_size = sample_size,
                       k = k,
                       es_type = es_type,
                       hg = hg,
                       model = model,
                       test_type = test_type,
                       p = p,
                       sd = NULL,
                       df = compute_power_range(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance),
                       homo_test = NULL,
                       homo_range = NULL)
    attr(power_list, "class") <- "mpower"


    } else if (missing(sd) & model =="random"){
      power_list <- list(variance = variance,
                         power = compute_power(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p),
                         effect_size = effect_size,
                         sample_size = sample_size,
                         k = k,
                         es_type = es_type,
                         hg = hg,
                         model = model,
                         test_type = test_type,
                         p = p,
                         sd = NULL,
                         df = compute_power_range(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance),
                         homo_test = homogen_mpower(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p),
                         homo_range = compute_homogen_range(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance))
      attr(power_list, "class") <- "mpower"

  } else if (!missing(sd) & model == "fixed"){
    power_list <- list(variance = variance,
                       power = compute_power(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p),
                       effect_size = effect_size,
                       sample_size = sample_size,
                       k = k,
                       es_type = es_type,
                       hg = hg,
                       model = model,
                       test_type = test_type,
                       p = p,
                       sd = sd,
                       df = compute_power_range(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance),
                       homo_test = homogen_mpower(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p, sd),
                       homo_range = compute_homogen_range(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance))
    attr(power_list, "class") <- "mpower"

    } else if (!missing(sd) & model == "random"){
    stop("sd arguement is not required for random-effects models")
  }

  return(power_list)

}
