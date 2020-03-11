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
                   sd = sd){

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
  if(k < 0)
    stop("Number of studies must be greater than 0")
  if(missing(model))
    stop("Need to specify type of model")
  if(!(model %in% model_options))
    stop("Need to specify 'fixed' or 'random' effects model")
  if(!(test_type %in% test_type_options))
    stop("Need to specify one-tailed or two-tailed")

  if(es_type == "Correlation"){
    effect_size = 2*effect_size/sqrt(1-effect_size^2)
  }else if(es_type == "OR") {
    effect_size = effect_size*(sqrt(3)/pi)
    }

  power_list <- list(power = compute_power(effect_size, sample_size, k, hg, model, test_type, p),
                     effect_size = effect_size,
                     sample_size = sample_size,
                     k = k,
                     hg = hg,
                     model = model,
                     test_type = test_type,
                     p = p,
                     sd =sd,
                     df = compute_power_range(effect_size, sample_size, k, model, test_type, p = .05),
                     homo_test = homogen_mpower(effect_size, sample_size, k, hg, model, test_type, p, sd))
  attr(power_list, "class") <- "mpower"
  return(power_list)
}

compute_power <- function(effect_size, sample_size, k, hg, model, test_type, p){

  lambda <- 0
  # Compute statistical Power
  if (model == "fixed"){
    lambda <- (effect_size/sqrt(compute_variance(sample_size, effect_size)/k))
  } else if (model == "random"){
    if (missing(hg)){
      message("Error: Enter heterogenity value")
    } else{
        if(hg == "small"){
          tau2 <- (1/3)*compute_variance(sample_size, effect_size)
        } else if (hg == "medium"){
          tau2 <- compute_variance(sample_size, effect_size)
        } else if (hg == "large"){
          tau2 <- 3*compute_variance(sample_size, effect_size)
          }
      lambda <- effect_size/sqrt((tau2 + compute_variance(sample_size, effect_size))/k)
    }
  }
  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }

  power <- (1-pnorm(c_alpha - lambda)) + pnorm(-1*c_alpha - lambda)
  return(power)
}

compute_variance <- function(sample_size, effect_size){
  return(
    round(((sample_size+sample_size)/((sample_size)*(sample_size))) + ((effect_size^2)/(2*(sample_size+sample_size))),5))
}

compute_power_range <- function(effect_size, sample_size, k, model, test_type, p = .05){

  if(model == "fixed"){
    df <- tibble(es_v = rep(effect_size, times = 90),
                 n_v = rep(sample_size, times = 90),
                 k_v = rep(seq(1:30), times = 3))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v, model = model, test_type = test_type,p = p))
  } else if (model == "random"){
    df <- tibble(es_v = rep(effect_size,times = 90),
               n_v = rep(sample_size, times = 90),
               k_v = rep(seq(1:30), times = 3),
               hg_v = rep(c("small","medium","large"), each = 30))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v,df$hg_v,model = model, test_type = test_type,p = p)) %>%
      mutate_at(vars(hg_v), factor)
  }
return(df)
}



# ## Between Group Moderation
# groups = 3
# n = 15
# diff = .4
# x1 = 0
# x2 = .1
# x3 =.55
# x_v = c(x1,x2,x3)
# df = groups - 1
# p = .95
# ## fixed effects
#
# weight <- sum(rep(n-groups,n/groups))
# overall_mean <- mean(x_v)
# lambda_b <- sum(weight*(x_v-overall_mean)^2)
# c_alpha <- qchisq(p,df,0, lower.tail = TRUE)
#
# power <- (1 - pchisq(c_alpha,df,lambda_b,lower.tail = TRUE))
# ### random
# var <- .083
# tau <- .028
# var_s <- var+tau
# var_s <- .33
#
# weight_s = 1/sum(rep(1/var_s,n/groups))
# lambda_b_s <- sum(weight_s*(x_v-overall_mean)^2)
# power <- (1 - pchisq(c_alpha,df,lambda_b_s,lower.tail = TRUE))
#
# ## Withing group moderation
#
# #fixed
# weight <-1/(1/(n-groups))
# var <- round(sqrt(1/sum(rep(weight,n/groups))),2)
# sd_v = c(1,1,4) ## user specified
# lambda_w <- sum(rep(weight*(sd_v*var)^2,n/groups))
# c_alpha_w <- qchisq(p, n-groups,0,lower.tail = TRUE)
# power_w <- (1 - pchisq(c_alpha_w,n-groups,lambda_w,lower.tail = TRUE))
# #random





#
# #power <- (1- pgamma(chi_crit/a, shape = b/2, scale = 2))   #alpha = shape, beta = scale
#
#
#
# lambda = df * (1/1)  # tau / var
# a = 1+(lambda/(df + lambda))
# b = df + (lambda^2/(df + 2*lambda))
# x = chi_crit/a



## Methods



# # test of homogenity Fixed effects
# chi_crit <- qchisq(p = p,df = df, lower.tail=FALSE)
# lambda = df * (1/1)  # tau / var
# a = 1+(lambda/(df + lambda))
# b = df + (lambda^2/(df + 2*lambda))
# x = chi_crit/a
# power <- (1- pgamma(chi_crit/a, shape = b/2, scale = 2))   #alpha = shape, beta = scale
#
# # test of homogenity Random effects
# chi_crit <- qchisq(p = p,df = df, lower.tail=FALSE)
# lambda = df * (1/1)  # tau / var
# a = 1+(lambda/(df + lambda))
# b = df + (lambda^2/(df + 2*lambda))
# x = chi_crit/a
# power <- pchisq(q = chi_crit/(1 + (1/1)), df = df, lower.tail = FALSE)







