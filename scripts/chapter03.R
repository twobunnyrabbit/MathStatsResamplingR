library(resampledata3)
library(tidyverse)

dat <- c(30, 25, 20, 18, 21, 22)
expand.grid(30, 25, 20, 18, 21, 22)
choose(6, 3)


library(combinat)
combn(dat, 1)

utils::combn(letters[1:4], 12, simplify = TRUE)

combn(dat, 6)
utils::combn(dat, 1)
?combn

dat_permn <- permn(dat)

dat_permn_uniq <- dat_permn |> unique()
str(dat_permn)

length(dat_permn)

View(dat_permn)

Beerwings |> 
  head()

Beerwings |> 
  ggplot(aes(x=Hotwings, color=Gender)) +
  stat_ecdf()
  
Beerwings |> 
  group_by(Gender) |> 
  summarise(across(Hotwings, .fns=list(mean=mean, sd=sd, median=median, min=min, max=max)))
factorial(6)
choose(6, 3)

obs <- Beerwings |> 
  group_by(Gender) |> 
  summarise(means = mean(Hotwings)) |> 
  ungroup() |> 
  pull(means) |> 
  diff()

library(car)
Tapply(Hotwings ~ Gender, fun=mean, data=Beerwings) |> diff()

hotwings <- pull(Beerwings, Hotwings)
N <- 10^5
result <- map_dbl(1:N, \(x) {
  idx <- sample(30, 15, replace = FALSE)
  mean(hotwings[idx]) - mean(hotwings[-idx])
})
ggplot() + geom_histogram(aes(result), bins=8) +
  geom_vline(xintercept = obs, linetype="dashed")

(sum(result >= obs)+1)/(N+1)

# example 3.4 Verizon cable repair time ####

Verizon |> summary()

Verizon |> 
  ggplot(aes(sample=Time, color=Group)) +
  geom_qq() +
  geom_qq_line()

Verizon |> 
  ggplot(aes(x=Time, color=Group)) +
  stat_ecdf()

# mean repair times between the two groups
obs_vz <- Verizon |> 
  group_by(Group) |> 
  summarise(mean=mean(Time)) |> 
  pull(mean) |> set_names(c("CLEC", "ILEC")) |> 
  diff() |> 
  set_names("ILEC-CLEC")

verizon_time <- Verizon$Time
n <- length(verizon_time)
N <- 10^4-1
results_verizon <- map_dbl(1:N, \(x) {
  idx <- sample(n, size=1664, replace = FALSE)
  mean(verizon_time[idx]) - mean(verizon_time[-idx])
})

ggplot() + geom_histogram(aes(results_verizon), color='white', bins=8) +
  geom_vline(xintercept = obs_vz, linetype='dashed')

# proportion of sample means that are less than or equal to observed mean difference
(sum(results_verizon <= obs_vz)+1)/(N+1)

# 0.0183
# The probability of observing a difference of 8.1 or more extreme between the two groups assuming that
# there is no difference in repair times is 0.018.
# Is this difference due to chance? The chance, or probability, is 0.018 which is low. So this result
# is unlikely due to chance, so we can conclude that there is a real difference between
# the repair times for the two groups.

# Example 3.6 #####

Verizon |> 
  ggplot(aes(x=Group, y=Time)) +
  geom_boxplot()

# differences in median
obs_vz_median <- Verizon |> 
  group_by(Group) |> 
  summarise(median=median(Time)) |> 
  pull(median) |> set_names(c("CLEC", "ILEC")) |> 
  diff() |> 
  set_names("ILEC-CLEC")

verizon_time <- Verizon$Time
n <- length(verizon_time)
N <- 10^4-1
results_verizon_median <- map_dbl(1:N, \(x) {
  idx <- sample(n, size=1664, replace = FALSE)
  median(verizon_time[idx]) - median(verizon_time[-idx])
})

ggplot() + geom_histogram(aes(results_verizon_median), color='white') +
  geom_vline(xintercept = obs_vz_median, linetype='dashed')


# Example 3.8 ####
Recidivism |> summary()

# get observed difference in proportions
obs_recid <- Recidivism |> 
  select(Age25, Recid) |> 
  drop_na() |> 
  table() |> 
  proportions(1) |>
  _[,2] |> 
  diff() |> 
  set_names(c('under_25-over_25'))

# sampling process 
# extract recid
recid <- Recidivism$Recid

group_sizes <- Recidivism |> 
  drop_na(Age25) |> 
  count(Age25) |> 
  pull(n) |> 
  set_names(c("over_25", "under_25"))

group_sizes['under_25']
total_group_size <- sum(group_sizes)
N <- 10^4-1

result_recid <- map_dbl(1:N, \(x) {
  idx <- sample(total_group_size, group_sizes['under_25'], replace = FALSE)
  mean(recid[idx] == "Yes") - mean(recid[-idx] == "Yes")
})

ggplot() +
  geom_histogram(aes(x=result_recid)) +
  geom_vline(xintercept = obs_recid, linetype="dashed")

(sum(result_recid >= obs_recid) + 1) / (N+1) * 2 # for two sided test

# Example 3.9 ####
gen_z <- round(257*0.41)
gen_mill <- round(2094*0.46)
obs_gen <- 0.46-0.41

pooled_data <- rep(c(1,0), c(1068, 1283))

N <- 10^4 - 1

results_gen <- map_dbl(1:N, \(x) {
  idx <- sample(2351, 2094, replace=FALSE)
  mean(pooled_data[idx]) - mean(pooled_data[-idx])
})

results_gen |> head()

ggplot() +
  geom_histogram(aes(x=results_gen)) +
  geom_vline(xintercept = obs_gen, linetype="dashed")

(sum(results_gen >= obs_gen) + 1 )/(N+1) * 2

# 3.4 ####
Diving2017
change <- Diving2017$Final - Diving2017$Semifinal
obs_diving <- mean(change)
N <- 10^4 - 1
result_diving <- map_dbl(1:N, \(x) {
  Sign <- sample(c(-1,1), 12, replace = TRUE)
  mean(Sign*change)
})

ggplot() +
  geom_histogram(aes(x=result_diving)) +
  geom_vline(xintercept = obs_diving, linetype="dashed")

(sum(result_diving >= obs_diving) + 1 )/(N+1) * 2
