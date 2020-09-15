#' @export

summary.mod_power <- function(object,...) {
  cat("\n Power Analysis for Categorical Moderator Analysis: \n\n")

  cat(" Number of Groups:                 ", object$n_groups, "\n")
  cat(" Groups:                           ", object$group, "\n")
  cat(" Expected Effect Sizes:            ", object$effect_sizes, "\n")
  cat(" Expected Sample Size (per group): ", object$sample_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Esimated Power: Moderator Analysis\n\n")

  cat(" Fixed-Effects Model:                 ", object$mod_power$fixed_power_b, "\n")
  cat(" Random-Effects Model (i2 = 0%):      ", object$mod_power$random_power_b_0, "\n")
  cat(" Random-Effects Model (i2 = 25%):     ", object$mod_power$random_power_b_s, "\n")
  cat(" Random-Effects Model (i2 = 50%):     ", object$mod_power$random_power_b_m, "\n")
  cat(" Random-Effects Model (i2 = 75%):     ", object$mod_power$random_power_b_l, "\n")
}
