#' Compute Power for Categorical Moderation Meta-analysis
#'
#' Computes statistical power for categorical moderator models under fixed- and random-effects models
#'
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

  ## Arguement Integrity Checks
  #model_options <- c("fixed", "random")
  test_type_options <- c("one-tailed", "two-tailed")
  #hg_options <- c("small", "medium", "large")
  es_type_options <- c("d","Correlation", "OR")

  #n_groups
  if(missing(n_groups))
    stop("Must specify number of groups: n_groups")
  if(!(is.numeric(n_groups)))
    stop("n_groups must be a single integer")
  if(length(n_groups) > 1)
    stop("n_groups must be a single integer")
  if(n_groups < 2)
    stop("number of groups must be at least 2")

  #effect_sizes
  if(missing(effect_sizes))
    stop("Need to specify expected effect sizes per group")
  if(length(effect_sizes) != n_groups)
    stop("The number of of effect sizes should match the number of groups")

  #sample_size
  if(missing(sample_size))
    stop("Need to specify expected sample size")
  if(!(is.numeric(sample_size)))
    stop("sample_size must be numeric")
  if(length(sample_size) > 1)
    stop("sample_size must be a single number")
  if(sample_size < 1)
    stop("sample_size must be greater than 0")

  # Number of Studies
  if(missing(k))
    stop("Need to specify expected number of studies")
  if(!(is.numeric(k)))
    stop("k must be numeric")
  if(length(k) > 1)
    stop("k must be a single number")
  if(k < 2)
    stop("k must be greater than 1")
  if((k/n_groups)%%1!=0)
    stop("Number of studies must be a multiple of n_groups")

  ## es_type
  if(missing(es_type))
    stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")
  if(!(es_type %in% es_type_options))
    stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")

  ## effect size
  # d
  if(es_type == 'd' & any(effect_sizes > 10))
    warning("Are you sure at least one effect size is >10?")
  # Correlation
  if(es_type == 'Correlation' & any(effect_sizes > 1))
    stop("Correlations cannot be above 1")
  if(es_type == 'Correlation' & any(effect_sizes < 0))
    stop("Correlations must be above 0")
  # Odds Ratio
  if(es_type == 'OR' & any(effect_sizes <= 1))
    stop("Odds ratio should be above 1")
  if(es_type == "OR" & missing(con_table))
    stop("For Odds Ratio, must enter contigency table (cont_table)")
  if(es_type == "OR" & !missing(con_table)){
    if(length(con_table) != 4)
      stop("con_table must reflect a 2x2 contingency table with the form c(a,b,c,d). see documentation")
    if(sample_size != sum(con_table))
      stop("Entered sample size should equal the sum of the contigency table")
    }

  # model
  #if(missing(model))
  #  stop("Need to specify 'fixed' or 'random' effects model")
  #if(!(model %in% model_options))
  #  stop("Need to specify 'fixed' or 'random' effects model")

  ## check for arguements that are not required for fixed models
  #if(model == "fixed"){
  #  if (!missing(hg)){
  #    stop("Fixed-effects models assume no heterogenity")
  #  } else if (missing(hg)) {
  #    hg = NULL
  #  }
  #}

  ## random effects and heterogenity parameter
  #if(model == 'random'){
  #  if(missing(hg)){
  #    stop("Need to specify small, medium, or large heterogenity")
  #  } else if (!(hg %in% hg_options)){
   #   stop("Need to specify small, medium, or large heterogenity")
  #  }
  #}

  ## test_type errors
  if(!(test_type %in% test_type_options))
    stop("Need to specify two-tailed or one-tailed")

  ##sd_within
  if(!missing(sd_within)){
    if(length(sd_within) != n_groups)
      stop("The number of of effect sizes should match the number within-group standard deviations")
  }

  effect_diff <- effect_sizes - effect_sizes[1]

  if(es_type == "Correlation"){
    effect_sizes <- 0.5*log((1+effect_diff)/(1-effect_diff))
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
  attr(mod_power_list, "class") <- "modpower"
  return(mod_power_list)
}

