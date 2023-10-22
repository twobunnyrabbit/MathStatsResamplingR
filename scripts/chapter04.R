library(resampledata3)
library(tidyverse)

rbinom(10, 10, 0.5) |> mean()


 map_dbl(1:1000, \(x) rbinom(10, 2, 0.5) |> mean()) |> 
  hist()
 
 x <- c(3,4,6,6)
mean(x) 
sd(x)

# 4.3 CTL formula
#

# Example 4.7
gamma_clt <- map_dbl(1:10^3, \(x) {
  mean(rgamma(30, shape=5, rate=2))  
})

pnorm(2.4495, lower.tail = FALSE)

ggplot() +
  geom_histogram(aes(x=gamma_clt)) 



mean(gamma_clt)
sd(gamma_clt)
mean(gamma_clt > 3) 

rgamma(10000, shape=5, rate=2) |> 
  list() |> 
  set_names("gamma") |> 
  data.frame(x=_) |> 
  ggplot(aes(x=gamma)) +
  geom_density()


gamma_clt2 <- map_dbl(1:10^4, \(x) {
  mean(rgamma(17, shape=100, rate=5))  
})

ggplot() +
  geom_histogram(aes(x=gamma_clt2)) +
  geom_vline(xintercept = 21.9, color='red', linetype="dashed")

mean(gamma_clt2 > 21.9)
coint_300 <- rbinom(300, 1, p=0.5)
pbinom(160, size = 300, p=0.5)
qbinom()

# 4.3.1 CLT for Binomial data

1-pbinom(19, 64, p=0.28)
pbinom(25, 120, 0.3)

pbinom(170, 700, 0.236) - pbinom(150, 700, 0.236)

