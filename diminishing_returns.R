### 
### Author: Kurt Peters, IXIS Digital (kurt@ixisdigital.com)
### Tue Oct 16 14:40:38 2018 


# LIBRARIES ####
library(tidyverse); library(reshape2)
library(IXIS.Auto)


tibble(
  x = seq(0, 1, by = .01),
  y = 1 - exp(-5 * x)
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = .5) +
  scale_x_continuous("% Effort", labels = scales::percent, breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("% Output", labels = scales::percent, breaks = seq(0, 1, by = .1))



tibble(
  x = seq(1, 10, by = .1),
  y = x*(1 - exp(-10 / x^2)) #the power of x is like a task-switching penalty: the more goals you have, the more time is required to manage/balance them (overhead); i.e. you pay an increasing price for more goals, beyond just dividing capacity
) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()
  scale_x_continuous("% Effort", labels = scales::percent, breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("% Output", labels = scales::percent, breaks = seq(0, 1, by = .1))

  
  
tibble(
  x = seq(0, 10, by = .1),
  Fitness = 5 / (1 + exp(-1*x)), #young
  Energy = -.05 * (x-.5)^2 + 5, #young
  # fitness = 10 / (1 + exp(-.5*x)), #old
  # energy = -1 * (x)^2 + 15, #old
  ROI = Fitness * Energy / 4
) %>%
  melt(id.var = "x", variable.name = "Variable") %>%
  ggplot(aes(x = x, y = value, color = Variable)) +
  geom_line(size = 3, alpha = .7) +
  scale_x_continuous("Time", limits = c(0, 6), breaks = seq(0, 10, by = 1)) +
  scale_y_continuous("Value", limits = c(2, NA), breaks = seq(0, 10, by = 1)) +
  scale_color_manual(values = c("black", "gray", "red")) +
  theme_bw()
  
  

#zero-order stimulus repeated over time and AUC
set.seed(1234)
tibble(
  x = seq(0, 10, by = 1),
  r = rnorm(length(x), sd = .75),
  Stimulus = 2 + r,
  AUC = cumsum(Stimulus)
) %>%
  #melt(id.var = "x", variable.name = "Variable") %>%
  ggplot(aes(x = x)) +
  geom_area(aes(y = AUC), fill = "red", alpha = .5) +
  geom_line(aes(y = AUC), color = "red", size = 3, alpha = .7) +
  geom_bar(aes(y = Stimulus), stat = "identity", fill = "black", alpha = 1) +
  scale_x_continuous("Time", breaks = seq(0, 10, by = 1), minor_breaks = NULL) +
  scale_y_continuous("Effect", minor_breaks = NULL) +
  scale_color_manual(values = c("black", "gray", "red")) +
  theme_bw()
  
  