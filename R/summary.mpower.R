#' @export

summary.mpower <- function(object,...) {
  cat("\n Power Analysis for Meta-analysis \n\n")

  cat(" Expected Effect Size:             ", object$effect_size, "\n")
  cat(" Expected Sample Size (per group): ", object$sample_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Estimated Power: Mean Effect Size \n\n")

  cat(" Fixed-Effects Model               ", object$power$fixed_power, "\n")
  cat(" Random-Effects Model (i2 =  0% ): ", object$power$random_power_0, "\n")
  cat(" Random-Effects Model (i2 = 25% ): ", object$power$random_power_25, "\n")
  cat(" Random-Effects Model (i2 = 50% ): ", object$power$random_power_50, "\n")
  cat(" Random-Effects Model (i2 = 75% ): ", object$power$random_power_75, "\n")
}
