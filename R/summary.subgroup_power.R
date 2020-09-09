#' @export

summary.subgroup_power <- function(object,...) {
  cat("\n Power Analysis for Subgroup analysis: \n\n")

  cat(" Number of Groups:                 ", object$n_groups, "\n")
  cat(" Groups:                           ", object$group, "\n")
  cat(" Expected Effect Sizes:            ", object$effect_sizes, "\n")
  cat(" Expected Sample Size (per group): ", object$sample_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Esimated Power to detect subgroup differences \n\n")

  cat(" Fixed-Effects Model:                 ", object$subgroup_power$fixed_power_b, "\n")
  cat(" Random-Effects Model (i2 = 0%):      ", object$subgroup_power$random_power_b_0, "\n")
  cat(" Random-Effects Model (i2 = 25%):     ", object$subgroup_power$random_power_b_s, "\n")
  cat(" Random-Effects Model (i2 = 50%):     ", object$subgroup_power$random_power_b_m, "\n")
  cat(" Random-Effects Model (i2 = 75%):     ", object$subgroup_power$random_power_b_l, "\n")
}
