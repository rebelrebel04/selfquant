library(tidyverse)
library(ixtra)
library(ixplor)

cron.0 <- read_csv("./data/cronometer_export/dailySummary.csv")
dup(cron.0, paste(Date))

exer.0 <- read_csv("./data/cronometer_export/exercises.csv")
exer.1 <- 
  exer.0 %>% 
  group_by(Day) %>% 
  summarize(`Calories Burned` = sum(`Calories Burned`, na.rm = TRUE))

cron.1 <- 
  cron.0 %>% 
  filter(Completed) %>% 
  left_join(exer.1, by = c(Date = "Day")) %>% 
  mutate(
    NetCalories = `Energy (kcal)` + `Calories Burned`,
    BMR = 1688,
    CaloriesToBMR = `Energy (kcal)` / BMR,
    NetCaloriesToBMR = NetCalories / BMR,    
    Macros = `Carbs (g)` + `Protein (g)` + `Fat (g)`,
    PctPro = `Protein (g)` / Macros,
    PctCarbs = `Carbs (g)` / Macros,
    PctFat = `Fat (g)` / Macros
  )

cron.1 %>% 
  filter(Date > as.Date("2020-01-01") & Date < as.Date("2020-06-01")) %>% 
  select(
    Date, 
    NetCalories, `Energy (kcal)`, `Calories Burned`, CaloriesToBMR, 
    Macros, PctPro, PctCarbs, PctFat
  ) %>% 
  pivot_longer(!Date) %>% 
  ggplot(aes(x = Date, y = value, group = name)) +
  geom_line() +
  facet_wrap(~ name, scales = "free_y")

cron.1 %>% 
  filter(Date > as.Date("2020-01-01") & Date < as.Date("2020-06-01")) %>% 
  select(
    #Date, 
    NetCalories, `Energy (kcal)`, `Calories Burned`, CaloriesToBMR, NetCaloriesToBMR,
    Macros, PctPro, PctCarbs, PctFat
  ) %>% 
  #pivot_longer(!Date) %>% 
  GGally::ggpairs()

names(cron.1) <- make.names(names(cron.1))
set.seed(1234)
fit.rf <- 
  cron.1 %>% 
  filter(Date > as.Date("2020-01-01") & Date < as.Date("2020-06-01")) %>% 
  select(-Date, -NetCalories, -Calories.Burned, -Energy..kcal.) %>% 
  randomForest::randomForest(CaloriesToBMR ~ ., data = ., importance = TRUE)
fit.rf
plot(fit.rf)
summary(fit.rf)
randomForest::varImpPlot(fit.rf)

top_pred <- 
  fit.rf$importance %>% 
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  arrange(desc(`%IncMSE`)) %>% 
  head(10) %>% 
  pull(variable)

cron.1 %>% 
  filter(Date > as.Date("2020-01-01") & Date < as.Date("2020-06-01")) %>% 
  select(
    NetCalories, Energy..kcal., Calories.Burned, CaloriesToBMR,
    !!!top_pred
  ) %>% 
  #pivot_longer(!Date) %>% 
  GGally::ggpairs()

# princomp()

#TODO: look at "servings" data export from cronometer the same way:
#      are there specific foods that predict overeating?