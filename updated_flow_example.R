library(tidyverse)
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
247473,223014,172260,149338,127242,105715,79614,53660,
31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

sex <- c("f", "m")

Age = c(0, 1, seq(5, 100, by = 5))
data_in <- tibble(Age,Deaths,Exposures)
dt <- rpois(n = 22, lambda = 1000)
d1 <- rpois(n = 22, lambda = 100000)

data_in <- expand_grid(sex, data_in) %>%
  mutate(Deaths = ifelse(sex == "f", dt, Deaths),
         Exposures = ifelse(sex == "f", d1, Exposures)) %>%
  mutate(.id = sex) %>%
   mutate(Sex = sex) %>%
  dplyr::select(-sex)



ex1 <- smooth_flexible(
  data_in,
  variable     = "Exposures",
  rough_method = "auto",
  fine_method  = "none",
  constrain_infants = TRUE,
  age_out = "abridged",
  u5m     = NULL)


ex2 <- smooth_flexible(
  data_in,
  variable     = "Deaths",
  rough_method = "auto",
  fine_method  = "none",
  constrain_infants = TRUE,
  age_out = "abridged",
  u5m     = NULL)

# exposures
ex1$figures[[1]]
ex1$figures[[2]]

# deaths
ex2$figures[[1]]
ex2$figures[[2]]

# here is the smoothed data
new_in <- ex1$data %>% 
  full_join(ex2$data) %>% 
  mutate(sex = .id)

# lifetable
data_out <- lt_flexible(
  data_in = new_in
)

# summary
lt_summary(na.omit(data_out)) # this is simply because data is from my head


z <- lt_plot(data_in,
        data_out,
        extrapFrom = extrapFrom)


# men (same for women)
z$m[[1]]
z$m[[2]]
z$m[[3]]
z$m[[4]]

# plot
z$nMx[[1]][[1]]

# data
z$nMx[[1]][[2]]


