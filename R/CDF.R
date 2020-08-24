CDF <- function(t, k, ncp, i2){
  df <- k-1
  constant <- (1-i2)^0.5
  part1 <- pgamma(df*(1-i2)/2, df/2)*pnorm((t - ncp)*constant)
  part2 <- 2*df*integrate(func, Z=t, df=df, ncp= ncp, i2=i2,  lower=constant, upper=Inf, rel.tol = 0.00000000000001)$value
  return(part1+part2)
}
