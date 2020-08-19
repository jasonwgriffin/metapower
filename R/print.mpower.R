#' @export

print.mpower <- function(x,...) {

    cat("\n Power Analysis for Meta-analysis \n\n")

    cat(" Expected Effect Size:             ", x$effect_size, "\n")
    cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
    cat(" Expected Number of Studies:       ", x$k, "\n")

    cat(" Estimated Power: Mean Effect Size \n\n")

    cat(" Fixed-Effects Model                    ", x$fixed_power, "\n")

    cat(" Random-Effects Model (I2 =   0% ):     ", x$jackson_power$random_power_0, "\n")
    cat(" Random-Effects Model (I2 =  25% ):     ", x$jackson_power$random_power_25, "\n")
    cat(" Random-Effects Model (I2 =  50% ):     ", x$jackson_power$random_power_50, "\n")
    cat(" Random-Effects Model (I2 =  75% ):     ", x$jackson_power$random_power_75, "\n")
    cat(" Random-Effects Model (I2 = 100% ):     ", x$jackson_power$random_power_100, "\n\n")
}
