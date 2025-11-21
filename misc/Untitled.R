library(tidyverse)
library(wmisc)
library(nlme)

dat <- read_csv("~/Documents/esp-pop-2024.csv")

dat %>% group_by(Year) %>%
  summarise(
    Max = max(Cites, na.rm = TRUE),
    Median = median(Cites, na.rm = TRUE),
    MAD = mad(Cites, na.rm = TRUE),
    Mean = mean(Cites, na.rm = TRUE),
    SD = sd(Cites, na.rm = TRUE)
          ) %>% 
  nice_table(round = 1)


fit <- glm(Cites ~ I(2024 - Year), data = dat, family = "poisson")
summary(fit)
nice_regression_table(fit)


