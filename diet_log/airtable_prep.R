### Prep csv data exported from Airtable diet log
### Author: Kurt Peters, IXIS Digital (kurt@ixisdigital.com)
### Sat Feb 12 17:07:16 2022 


# LIBRARIES ####
library(tidyverse)


# READ ####
food.0 <- read_csv("./diet_log/airtable_exports/FoodLog-Grid view.csv", name_repair = "universal")
poop.0 <- read_csv("./diet_log/airtable_exports/PoopLog-Grid view.csv", name_repair = "universal")


# So basically I have a fuzzy key, where I want to join inputs (food)
# with outputs (poop) that were timestamped within 24-36 hours later
# (note: average colon transit time is ~30 hours)

# Ultimately I'll want a wide dataset keyed by timestamp of each input
# So will have multiple potential DVs as well as features (foods) wide


# POC: process for converting the comma-separated list in the Veg field
# into wide (binary) features -- can then do same for other fields

extract_features <- function(x) {
  
  x[is.na(x)] <- "none"
  
  x_clean <- 
    x %>% 
    map(~ unlist(str_split(.x, ",")) %>% 
          trimws() %>% 
          tolower() %>% 
          gsub("\\W", "", .)
    )
  
  x_unique <- unique(unlist(x_clean))
  cli::cli_alert_info(paste(x_unique, collapse = ", "))
  
  x_clean %>% 
    map_dfr(~ setNames(x_unique %in% .x, x_unique)) %>% 
    select(!any_of("none"))
  
}

# extract_features(food.0$Veg)
# extract_features(food.0$Fruit)
# extract_features(food.0$Protein)
# extract_features(food.0$Nuts)


# IVs ####
food.1 <- 
  food.0 %>% 
  select(Veg, Fruit, Protein, Nuts, Extras, Snacks, Follow.ups) %>% 
  map_dfc(extract_features) %>% 
  bind_cols(
    food.0 %>% 
      mutate(
        dim_date = as.POSIXct(strptime(Date, "%F %I:%M%p")),
        veg_cooked = ifelse(Veg.Type == "cooked", TRUE, FALSE)
      ) %>% 
      select(dim_date, veg_cooked)
  ) %>% 
  select(dim_date, veg_cooked, everything()) %>% 
  glimpse()


food.1 %>% 
  pivot_longer(!dim_date) %>% 
  ggplot(aes(x = dim_date, y = reorder(name, value), fill = value)) +
  geom_tile()


# DVs ####
poop.1 <- 
  poop.0 %>% 
  mutate(
    dim_date = as.POSIXct(strptime(Date, "%m/%d/%Y %I:%M%p")),    
    across(c(Speed, Quality, Quantity, Color), ~ tolower(trimws(gsub("\\W", "", .x))))
  ) %>% 
  bind_cols(extract_features(poop.0$Checks)) %>% 
  select(-Date, -Checks, -Notes) %>% 
  select(dim_date, everything()) %>% 
  glimpse()

# > DIM REDUCTION ####
# Possible to reduce the DVs into 1 component?

poop.2 <- 
  poop.1 %>% 
  mutate(
    Speed_const = Speed == "constipated",
    Quality_notgood = Quality != "good",
    Quantity_high = Quantity == "high",
    Color_notnormal = Color != "normal"
  ) %>% 
  select(-Speed, -Quality, -Quantity, -Color) %>% 
  mutate(across(!dim_date, as.integer)) %>% 
  glimpse()

poop.2 %>% 
  select(-dim_date) %>% 
  filter(!is.na(Wipes)) %>% 
  corrr::correlate() %>% 
  corrr::rplot(print_cor = TRUE)

fit.pca <- 
  poop.2 %>% 
  select(-dim_date) %>% 
  filter(!is.na(Wipes)) %>% 
  mutate(Wipes = scale(Wipes)[,1]) %>% 
  princomp()
fit.pca
plot(fit.pca)
biplot(fit.pca) #so higher values on pca1 are worse quality

poop.3 <- 
  poop.2 %>% 
  filter(!is.na(Wipes)) %>% 
  mutate(pca1 = fit.pca$scores[, 1]) %>% 
  glimpse()





# JOIN ####
# NOTE: the filter on delta below enforces the assumption about 
# transit time, ie, the window in which a given meal is assumed
# to impact poop quality
fuzzy_keys <- 
  expand.grid(
    food = food.1$dim_date,
    poop = poop.3$dim_date
  ) %>% 
  mutate(
    delta = difftime(poop, food, units = "hours")
  ) %>% 
  filter(delta > 12 & delta < 40) %>% 
  arrange(food, delta) %>% 
  group_by(food) %>% 
  slice_head(n = 1) %>% 
  ungroup()

food.1 %>% anti_join(fuzzy_keys, by = c("dim_date" = "food"))

poop.1 %>% anti_join(fuzzy_keys, by = c("dim_date" = "poop"))

all.1 <- 
  food.1 %>% 
  left_join(
    fuzzy_keys,
    by = c("dim_date" = "food")
  ) %>% 
  left_join(
    poop.3,
    by = c("poop" = "dim_date")
  ) %>% 
  # mutate(
  #   Quality = factor(Quality, levels = c("soft", "mediocre", "good"), ordered = TRUE)
  # ) %>% 
  filter(!is.na(poop)) %>% 
  glimpse()

summary(all.1$pca1)
summary(all.1$Wipes)
summary(all.1$Quality)



# INIT MODELING ####
# Just toying around to start...
fit.rf <- 
  all.1 %>% 
  filter(!is.na(Wipes)) %>% 
  ranger::ranger(Wipes ~ . -dim_date -poop -pca1, data = ., importance = "impurity")

fit.rf <- 
  all.1 %>% 
  ranger::ranger(pca1 ~ . -delta -dim_date -poop -Wipes -ghostpoopy -complete -gassy.y -bloated -Speed_const -Quality_notgood -Quantity_high -Color_notnormal, data = ., importance = "impurity")

fit.rf <-
  all.1 %>%
  filter(!is.na(Quality)) %>%  
  mutate(
    Quality_notgood = Quality != "good"
    # Quantity_high = Quantity == "high"
  ) %>% 
  ranger::ranger(Quality_notgood ~ . -Quality -pca1 -dim_date -poop -Wipes, data = ., importance = "impurity")

fit.rf
ranger::importance(fit.rf) %>% 
  as_tibble(rownames = "pred") %>% 
  ggplot(aes(x = reorder(pred, value), y = value)) +
  geom_bar(stat = "identity") +
  coord_flip()





# UNIVARIATE ####

# Contingency tables for categorical variables
prop.table(table(all.1$peanutbutter, all.1$Quality))
prop.table(table(all.1$chocolate, all.1$Quality))
prop.table(table(all.1$cabbage, all.1$Quality))
prop.table(table(all.1$eggshardboiled, all.1$Quality))

all.1 %>% 
  split(.$peanutbutter) %>% 
  map(~ summary(.$Wipes))

all.1 %>% 
  split(.$peanutbutter) %>% 
  map(~ summary(.$pca1))

all.1 %>% 
  mutate(
    Quality_notgood = Quality != "good",
    Quantity_high = Quantity == "high"
  ) %>% 
  select(dim_date, veg_cooked:tortillas, pca1, Wipes, Quality_notgood, Quantity_high) %>% 
  pivot_longer(!c(dim_date, pca1, Wipes, Quality_notgood, Quantity_high), names_to = "food", values_to = "present") %>% 
  group_by(food, present) %>% 
  summarize(
    across(c(pca1, Wipes, Quality_notgood, Quantity_high), mean, na.rm = TRUE)
  ) %>% 
  filter(present) %>% 
  pivot_longer(!c(food, present)) %>% 
  
  ggplot(aes(x = reorder(food, value), y = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ name, scales = "free_x")
