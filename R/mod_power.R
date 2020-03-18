#' Compute Power for Categorical Moderation Meta-analysis
#'
#' mod_power( ) is an extension of mpower() and so it takes similiar inputs. The primary inputs are the effected effect sizes
#' for each group and the expected within-group standard deviation in those groups
#'
#' @param n_groups Number of anticipated groups in moderation analysis.
#'
#' @param effect_sizes  Expected effect sizes of for each group.
#'
#' @param es_type Effect size metric: 'Correlation', 'd', or 'OR'.
#'
#' @param sample_size Expected number of participants per group.
#'
#' @param k Total expected number of studies.
#'
#' @param hg Expected heterogenity estimate (small, mdeium, large)
#'
#' @param model Fixed-effects model (model = "fixed") or Random-effects model (model = "random")
#'
#' @param p alpha level: p = .05 (DEFAULT)
#'
#' @param test_type one-tailed or two-tailed
#'
#' @param sd_within standard deviation within groups
#'
#' @return Estimated Power estimates for between and within-groups moderation
#'
#' @examples
#' mod_power(
#'  n_groups = 3,
#'  effect_sizes = c(0,.1,.55),
#'  sample_size = 15,
#'  k = 15,
#'  model = "random",
#'  hg = "small",
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
                      model,
                      hg,
                      p,
                      es_type,
                      test_type,
                      sd_within) {

  if(es_type == "d"){
    effect_sizes <- effect_sizes/sqrt(effect_sizes^2 + (sample_size + sample_size)^2/(sample_size*sample_size))
    #effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes))
  }else if(es_type == "OR") {
    effect_sizes <- effect_sizes*(sqrt(3)/pi)
    effect_sizes <- effect_sizes/sqrt(effect_sizes^2 + (sample_size + sample_size)^2/(sample_size*sample_size))
    #effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes))
  }

  effect_diff <- effect_sizes - effect_sizes[1]

  if(es_type == "d"){
    effect_diff <- 0.5*log((1+effect_diff)/(1-effect_diff))
  }else if(es_type == "OR") {
    effect_diff <- effect_diff*(sqrt(3)/pi)
    effect_diff <- effect_diff/sqrt(effect_diff^2 + (sample_size + sample_size)^2/(sample_size*sample_size))
    effect_diff <- 0.5*log((1+effect_diff)/(1-effect_diff))
  } else if (es_type == "Correlation") {
    effect_diff <- 0.5*log((1+effect_diff)/(1-effect_diff))
  }

  overall_effect_diff <- mean(effect_diff)

  df_b <- n_groups-1
  df_w <- k-n_groups

  if(test_type == "two-tailed"){
    c_alpha_b <- qchisq(1-(p/2),df_b,0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-(p/2), df_w,0,lower.tail = TRUE)
    } else if(test_type == "one-tailed"){
      c_alpha_b <- qchisq(1-p,df_b,0, lower.tail = TRUE)
      c_alpha_w <- qchisq(1-p, df_w,0,lower.tail = TRUE)
  }

  if(model == "fixed") {
    hg = NA
    ## between groups
    weight <- sum(rep(sample_size-n_groups,sample_size/n_groups))
    #weight <- sum(rep(compute_variance(sample_size,overall_effect_diff)/k,k/n_groups))
    lambda_b <- sum(weight*(effect_diff-overall_effect_diff)^2)
    power_b = 1 - pchisq(c_alpha_b,df_b,lambda_b,lower.tail = TRUE)
    ##within-groups
    weight_w <-1/(1/(sample_size-n_groups))
    var_w <- round(sqrt(1/sum(rep(1/(1/(sample_size-3)),sample_size/n_groups))),2)
    lambda_w <- sum(rep(weight_w*(sd_within*var_w)^2, sample_size/n_groups))
    power_w <- 1 - pchisq(c_alpha_w,df_w,lambda_w,lower.tail = TRUE)

    } else if(model =="random") {
     if(hg == "small"){
       tau2 <- (1/3)*(1/(sample_size - 3))
     } else if (hg == "medium"){
       tau2 <- (1)*(1/(sample_size - 3))
     } else if (hg == "large"){
       tau2 <- (3)*(1/(sample_size - 3))
     }
     ## between groups
     weight_b <- 1/sum(rep(1/(1/(sample_size-3)+tau2),sample_size/n_groups))
     lambda_b <- sum(weight_b*(effect_diff-overall_effect_diff)^2)
     power_b = 1 - pchisq(c_alpha_b,df_b,lambda_b,lower.tail = TRUE)
     lambda_w <- NULL
     power_w <- NA
     }

  power_list <- list(power_b = power_b,
                     power_w = power_w,
                     effect_sizes = effect_sizes,
                     effect_diff = effect_diff,
                     sample_size = sample_size,
                     k = k,
                     model = model,
                     hg = hg,
                     n_groups = n_groups,
                     es_type = es_type,
                     df_b = df_b,
                     df_w = df_w,
                     lambda_b = lambda_b,
                     c_alpha_b = c_alpha_b,
                     lambda_w = lambda_w,
                     c_alpha_w = c_alpha_w,
                     overall_effect_diff = overall_effect_diff)
  attr(power_list, "class") <- "modpower"
  return(power_list)
}

