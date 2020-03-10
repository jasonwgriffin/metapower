homogen_mpower <- function (){


p = .05
sample_size = 20
k = 10
sd = .5
effect_size = .2
tau = .3
## fixed effects test of homogenity
lambda <- (k/compute_variance(sample_size, effect_size))*(compute_variance(sample_size, effect_size)/k)*(sd^2)
weight = 1/round(compute_variance(sample_size, effect_size),2)
df <- k-1
c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
power <- (1 - pchisq(c_alpha,df,lambda,lower.tail = TRUE))
## random effects test of homogenity
#t <- 3*compute_variance(n,es)
c <- k*weight- k*weight^2/(k*weight)
u_q <- c*tau+df
sd_q <- 2*df + 4*c*(tau^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau^4
r = sd_q/(2*u_q)
s = 2*u_q^2/sd_q
power <- (1 - pchisq(c_alpha/r,s,ncp = 0, lower.tail = TRUE))
}
