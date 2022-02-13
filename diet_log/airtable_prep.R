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


# JOIN ####
fuzzy_keys <- 
  expand.grid(
    food = food.1$dim_date,
    poop = poop.1$dim_date
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
    poop.1,
    by = c("poop" = "dim_date")
  ) %>% 
  filter(!is.na(poop)) %>% 
  glimpse()

summary(all.1$Wipes)

fit.rf <- 
  all.1 %>% 
  filter(!is.na(Wipes)) %>% 
  ranger::ranger(Wipes ~ . -dim_date -poop, data = ., importance = "impurity")

fit.rf
ranger::importance(fit.rf) %>% 
  as_tibble(rownames = "pred") %>% 
  ggplot(aes(x = reorder(pred, value), y = value)) +
  geom_bar(stat = "identity") +
  coord_flip()
