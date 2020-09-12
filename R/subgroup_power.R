#' Compute Power for Subgroup Analysis in Meta-analysis
#'
#' Computes statistical power for different subgroups under fixed and random effects models.
#'
#' @param n_groups Numerical value for the number of subgroups.
#'
#' @param effect_sizes Numerical values for effect sizes of for each group.
#'
#' @param es_type Character reflecting effect size metric: 'r', 'd', or 'or'.
#'
#' @param sample_size Numerical value for number of participants (per study).
#'
#' @param k Numerical value for total number of studies.
#'
#' @param p Numerical value for significance level (Type I error probability).
#'
#' @param con_table (Optional) List of numerical values for 2x2 contingency
#'     tables as a vector in the following format: c(a,b,c,d). These should be
#'     specified for each subgroup (i.e., n_groups).
#'
#' \tabular{lcc}{
#'  2x2 Table   \tab Group 1 \tab Group 2 \cr
#'  Present     \tab a       \tab b       \cr
#'  Not Present \tab c       \tab d       \cr
#'}
#' @return Estimated Power estimates for subgroup differences under fixed- and random-effects models
#' @examples
#' subgroup_power(n_groups = 2,
#'                effect_sizes = c(.1,.5),
#'                sample_size = 20,
#'                k = 10,
#'                es_type = "d")
#' subgroup_power(n_groups = 2,
#'                con_table = list(g1 = c(6,5,4,5), g2 = c(8,5,2,5)),
#'                sample_size = 40,
#'                k = 20,
#'                es_type = "or")
#'
#' @seealso
#' \url{https://jason-griffin.shinyapps.io/shiny_metapower/}
#'
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @importFrom stats dchisq
#' @importFrom stats integrate
#' @importFrom stats pgamma
#' @export

subgroup_power <- function(n_groups, effect_sizes, sample_size, k, es_type, p = .05, con_table = NULL) {

  if(missing(effect_sizes))
    effect_sizes = NULL
  ## Argument Check
  subgroup_power_integrity(n_groups, effect_sizes, sample_size, k, es_type, p, con_table)

  ## compute degrees of freedom for between and within-study
  df_b <- n_groups-1
  df_w <- k-n_groups
  ## compute critical value for power
  c_alpha_b <- qchisq(1-p, df_b, 0, lower.tail = TRUE)
  c_alpha_w <- qchisq(1-p, df_w, 0, lower.tail = TRUE)

  ## factor by which range of studies will population power curves
  range_factor <- 5

  if(es_type == "d"){
    sample_size <- sample_size/2
    effect_diff <- effect_sizes - effect_sizes[1] # difference in effects
    overall_effect <- mean(effect_sizes) # find overall mean
    variance <- compute_variance(sample_size/n_groups, overall_effect, es_type, con_table) # compute variance for each group sample_size/2

    group = NULL

    # create a power range of data

    subgroup_power_range_df <- data.frame(k_v = seq(from = n_groups, to = range_factor*k, by = n_groups),
                                 overall_effect = overall_effect,
                                 n_groups = n_groups,
                                 n_v = sample_size/n_groups,
                                 c_alpha_b = c_alpha_b,
                                 c_alpha_w = c_alpha_w) %>% dplyr::mutate(variance = mapply(compute_variance, .data$n_v, .data$overall_effect, es_type))

    }else if(es_type == "r"){
      effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes)) ## changes correlation to fisher's z
      effect_diff <- effect_sizes - effect_sizes[1] # difference in effects
      overall_effect <- mean(effect_sizes) # find overall mean
      variance <- compute_variance(sample_size/n_groups, overall_effect, es_type, con_table)
      group = NULL
      ##
      subgroup_power_range_df <- data.frame(k_v = seq(from = n_groups, to = range_factor*k, by = n_groups),
                                       overall_effect = overall_effect,
                                       n_groups = n_groups,
                                       n_v = sample_size,
                                       c_alpha_b = c_alpha_b,
                                       c_alpha_w = c_alpha_w) %>% dplyr::mutate(variance = mapply(compute_variance, .data$n_v, .data$overall_effect, es_type))

      }else if(es_type == "or") {

        ## gather user inputted group names
        group <- names(con_table)
        d <- data.frame(group)  # create a data.frame with length equal to how many groups the user enters
        d$a <- sapply(con_table, "[[", 1)  ## extract the 2x2 components c(a,b,c,d)
        d$b <- sapply(con_table, "[[", 2)
        d$c <- sapply(con_table, "[[", 3)
        d$d <- sapply(con_table, "[[", 4)
        d$or <- round((d$a*d$d)/(d$b*d$c),3) ## Compute Odds Ratio
        d$log_or <- round(log(d$or),3)  ## Convert to log odds
        d$var <- round((1/d$a) + (1/d$b) + (1/d$c) + (1/d$d),3) ## compute variance of log odds
        effect_diff <- d$log_or - d$log_or[1] ## compute anticipated difference among groups
        overall_effect <- mean(d$log_or) ## find overall mean
        variance <- mean(d$var) ## find the common variance among all groups
        effect_sizes <- d$log_or ## save the effect sizes in log odds to input in subsequent functions
        ## dataframe for applying subgroup_power()
        subgroup_power_range_df <- data.frame(k_v = seq(from = n_groups, to = range_factor*k, by = n_groups),
                                         overall_effect = overall_effect,
                                         n_groups = n_groups,
                                         n_v = sample_size,
                                         c_alpha_b = c_alpha_b,
                                         c_alpha_w = c_alpha_w,
                                         variance = variance)
  }

  subgroup_power_list <- list(subgroup_power = compute_subgroup_power(n_groups, effect_sizes, variance, overall_effect, sample_size, k, c_alpha_b),
                         subgroup_power_range = compute_subgroup_range(n_groups, effect_sizes, subgroup_power_range_df),
                         subgroup_power_range_df = subgroup_power_range_df,
                         n_groups = n_groups,
                         effect_sizes = effect_sizes,
                         sample_size = sample_size,
                         k = k,
                         es_type = es_type,
                         variance = variance,
                         group = group)
  attr(subgroup_power_list, "class") <- "subgroup_power"
  return(subgroup_power_list)
}
