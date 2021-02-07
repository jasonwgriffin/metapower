#' @export

summary.mpower <- function(object,...) {

  cat("\n Power Analysis for Meta-analysis \n\n")

  cat(" Effect Size Metric:               ", object$es_type, "\n")
  cat(" Expected Effect Size:             ", object$effect_size, "\n")
  cat(" Expected Study Size:              ", object$study_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Estimated Power: Mean Effect Size \n\n")

  cat(" Fixed-Effects Model               ", object$power$fixed_power, "\n")
  cat(" Random-Effects Model (i2 = ",round(object$i2*100,2), "%):   ", object$power$random_power, "\n", sep = "")

}
