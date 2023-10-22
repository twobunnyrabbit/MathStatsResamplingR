library(tidyverse)
library(resampledata3)

x <- c(17.7, 22.6, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
df <- data.frame(x = x)
ggplot(df, aes(sample = x)) + geom_qq() + geom_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

ggplot(NCBirths2004, aes(sample = Weight))+ geom_qq() +
  geom_qq_line() +
  facet_wrap(~Smoker)

# empirical cumulative distribution
x <- c(3, 6, 15, 15, 17, 19, 24)
df <- data.frame(x = x)
ggplot(df, aes(x = x)) + stat_ecdf()

df <- data.frame(x = rnorm(25)) # random sample from N(0,1)
ggplot(df, aes(x = x)) + stat_ecdf() +
  stat_function(fun = pnorm, color = "blue") +
  labs(x="", y = "F(x)")

# hot wings study
ggplot(Beerwings, aes(x = Beer, linetype = Gender, color = Gender)) +
  stat_ecdf() + labs(x = "Beer (oz)", y = "F(x)") + 
  geom_vline(xintercept = 25, lty = 3) +
  scale_linetype_manual(values = c(1,5))

# relationship between the number of wings eaten and beers consumed
Beerwings |> head()
Beerwings |> summary()
Beerwings |> 
  ggplot(aes(x=Hotwings, y=Beer, color=Gender)) +
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE)

## Exercises
