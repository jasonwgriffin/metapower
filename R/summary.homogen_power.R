#' @export

summary.homogen_power <- function(object,...) {

  cat("\n Power Analysis for Test of Homogeneity in Meta-analysis \n\n")

  cat(" Effect Size Metric:               ", object$es_type, "\n")
  cat(" Expected Effect Size:             ", object$effect_size, "\n")
  cat(" Expected Study Size:              ", object$study_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Estimated Power: Test of Homogeneity \n\n")

  cat(" Fixed-Effects Model (SD = 1)       ", object$homogen_power$fixed_power_sd1, "\n")
  cat(" Fixed-Effects Model (SD = 2)       ", object$homogen_power$fixed_power_sd2, "\n")
  cat(" Fixed-Effects Model (SD = 3)       ", object$homogen_power$fixed_power_sd3, "\n")
  cat(" Fixed-Effects Model (SD = 4)       ", object$homogen_power$fixed_power_sd4, "\n")
  cat(" Fixed-Effects Model (SD = 5)       ", object$homogen_power$fixed_power_sd5, "\n\n")

  cat(" Random-Effects Model (i2 = ",round(object$i2*100,2), "%):    ", object$homogen_power$random_power, "\n", sep = "")

}
