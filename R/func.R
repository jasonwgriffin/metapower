func <- function (x, Z, df, ncp, i2)
{
  x*pnorm(Z*x-ncp*(1-i2)^0.5) * dchisq(df* x^2, df)
}
