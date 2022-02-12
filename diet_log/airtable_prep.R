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

food.0 %>% 
  select(Veg, Fruit, Protein, Nuts, Extras, Snacks) %>% 
  map_dfc(extract_features) %>% 
  bind_cols(
    food.0 %>% 
      select(Date, Veg.Type, Follow.ups)
  ) %>% 
  select(Date, everything()) %>% 
  glimpse()



