library(tidyverse)



set.seed(7272)
big.tibble = tibble(x1=rbinom(10000, 1004, 0.39)/1004,x2=rbinom(10000,2008,.39)/2008) 
 
quantile(big.tibble$x1,0.975)

((quantile(big.tibble$x1,0.975) - quantile(big.tibble$x1,0.025))/2)
((quantile(big.tibble$x2,0.975) - quantile(big.tibble$x2,0.025))/2)

ggplot(data=big.tibble)+
  geom_histogram(aes(x=x1, y = after_stat(density)),bins = 23)+
  geom_density(aes(x=x1))+
  theme_minimal()+
  ylab("Density")+
  xlab("Sample Proportions with Sample Size 1004")



ggplot(data=big.tibble)+
  geom_histogram(aes(x=x2, y = after_stat(density)),bins = 27)+
  geom_density(aes(x=x2))+
  theme_minimal()+
  ylab("Density")+
  xlab("Sample Proportions with Sample Size 2008")





survey = tibble(data = c(rep(1, times = 392),rep(0,times=592),rep(-1,times=20)))

resampled = c()

for(i in 1:10000){
  current.sample = sample(survey$data,1004,replace = T)
  
  resampled[i] = length(which(current.sample == 1))/length(current.sample)
}

big.tibble = big.tibble |>
  mutate(resampled = resampled)

((quantile(big.tibble$resampled,0.975) - quantile(big.tibble$resampled,0.025))/2)

ggplot(data = big.tibble)+
  geom_histogram(aes(x=resampled,y=after_stat(density)),bins = 29)+
  geom_density(aes(x=resampled))+
  theme_minimal()+
  xlab("Proportions")+
  ylab("Density")




simu.results <- tibble(n = numeric(), p = numeric(), range = numeric())


for(n in seq(100, 3000, 10)){
  for(p in seq(0.01, 0.99, 0.01)){
    set.seed(7272)
    current = rbinom(10000, n, p) / n
    range = (quantile(current, 0.975) - quantile(current, 0.025)) / 2
    simu.results <- bind_rows(simu.results, tibble(n = n, p = p, range = range)) |>
      unique()
  }
}

ggplot(data = simu.results) +
  geom_raster(aes(x = n, y = p, fill = range)) +
  xlab("Sample Size") +
  ylab("Proportion") +
  theme_minimal() +
  scale_fill_viridis_c(name = "Margin of Error")

form.results <- tibble(n = numeric(), p = numeric(), range = numeric())

z = qnorm(0.975)
for(n in seq(100,3000,10)){
  for(p in seq(0.01, 0.99, 0.01)){
    set.seed(7272)
    range = z*sqrt(n*p*(1-p) + z/4)/(n+z^2)
    form.results <- bind_rows(form.results, tibble(n = n, p = p, range = range)) |>
      unique()
  }
}

ggplot(data = form.results)+
  geom_raster(aes(x=n,y=p, fill = range))+
  xlab("Sample Size")+
  ylab("Proportion")+
  theme_minimal() +
  scale_fill_viridis_c(name = "Margin of Error")
                 