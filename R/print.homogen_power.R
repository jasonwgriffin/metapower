#' @export

print.homogen_power <- function(x,...) {

  cat("\n Power Analysis for Test of Homogeneity in Meta-analysis \n\n")

  cat(" Effect Size Metric:               ", x$es_type, "\n")
  cat(" Expected Effect Size:             ", x$effect_size, "\n")
  cat(" Expected Study Size:              ", x$study_size, "\n")
  cat(" Expected Number of Studies:       ", x$k, "\n\n")

  cat(" Estimated Power: Test of Homogeneity \n\n")

  cat(" Fixed-Effects Model (SD = 1)       ", x$homogen_power$fixed_power_sd1, "\n")
  cat(" Fixed-Effects Model (SD = 2)       ", x$homogen_power$fixed_power_sd2, "\n")
  cat(" Fixed-Effects Model (SD = 3)       ", x$homogen_power$fixed_power_sd3, "\n")
  cat(" Fixed-Effects Model (SD = 4)       ", x$homogen_power$fixed_power_sd4, "\n")
  cat(" Fixed-Effects Model (SD = 5)       ", x$homogen_power$fixed_power_sd5, "\n\n")

  cat(" Random-Effects Model (i2 = ",round(x$i2*100,2), "%):    ", x$homogen_power$random_power, "\n", sep = "")

}
