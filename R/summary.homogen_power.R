#' @export

summary.homogen_power <- function(object,...) {

  cat("\n Power Analysis for Test of Homogeneity in Meta-analysis \n\n")

  cat(" Expected Effect Size:             ", object$effect_size, "\n")
  cat(" Expected Sample Size (per group): ", object$sample_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Estimated Power: Test of Homogeneity \n\n")

  cat(" Fixed-Effects Model (SD = 1)       ", object$homogen_power$fixed_power_sd1, "\n")
  cat(" Fixed-Effects Model (SD = 2)       ", object$homogen_power$fixed_power_sd2, "\n")
  cat(" Fixed-Effects Model (SD = 3)       ", object$homogen_power$fixed_power_sd3, "\n")
  cat(" Fixed-Effects Model (SD = 4)       ", object$homogen_power$fixed_power_sd4, "\n")
  cat(" Fixed-Effects Model (SD = 5)       ", object$homogen_power$fixed_power_sd5, "\n\n")

  cat(" Random-Effects Model (i2 =   0% ): ", object$homogen_power$random_power_0, "\n")
  cat(" Random-Effects Model (i2 =  25% ): ", object$homogen_power$random_power_s, "\n")
  cat(" Random-Effects Model (i2 =  50% ): ", object$homogen_power$random_power_m, "\n")
  cat(" Random-Effects Model (i2 =  75% ): ", object$homogen_power$random_power_l, "\n\n")
}
