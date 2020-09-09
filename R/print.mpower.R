#' @export

print.mpower <- function(x,...) {
    cat("\n Power Analysis for Meta-analysis \n\n")

    cat(" Expected Effect Size:             ", x$effect_size, "\n")
    cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
    cat(" Expected Number of Studies:       ", x$k, "\n\n")

    cat(" Estimated Power: Mean Effect Size \n\n")

    cat(" Fixed-Effects Model               ", x$power$fixed_power, "\n")
    cat(" Random-Effects Model (i2 =  0% ): ", x$power$random_power_0, "\n")
    cat(" Random-Effects Model (i2 = 25% ): ", x$power$random_power_25, "\n")
    cat(" Random-Effects Model (i2 = 50% ): ", x$power$random_power_50, "\n")
    cat(" Random-Effects Model (i2 = 75% ): ", x$power$random_power_75, "\n")
}
