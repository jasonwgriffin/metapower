#' @export

print.mpower <- function(x,...) {
    cat("\n Power Analysis for Meta-analysis \n\n")

    cat(" Expected Effect Size:             ", x$effect_size, "\n")
    cat(" Expected Study Size:              ", x$study_size*2, "\n")
    cat(" Expected Sample Size (per group): ", x$study_size, "\n")
    cat(" Expected Number of Studies:       ", x$k, "\n\n")

    cat(" Estimated Power: Mean Effect Size \n\n")

    cat(" Fixed-Effects Model               ", x$power$fixed_power, "\n")
    cat(" Random-Effects Model (i2 = ",round(x$i2*100,2), "%):  ", x$power$random_power, "\n", sep = "")

}
