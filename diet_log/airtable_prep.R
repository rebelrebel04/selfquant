### Prep csv data exported from Airtable diet log
### Author: Kurt Peters, IXIS Digital (kurt@ixisdigital.com)
### Sat Feb 12 17:07:16 2022 


# LIBRARIES ####
library(tidyverse)


# READ ####
food.0 <- read_csv("./diet_log/airtable_exports/FoodLog-Grid view.csv")
poop.0 <- read_csv("./diet_log/airtable_exports/PoopLog-Grid view.csv")


# So basically I have a fuzzy key, where I want to join inputs (food)
# with outputs (poop) that were timestamped within 24-36 hours later
# (note: average colon transit time is ~30 hours)

# Ultimately I'll want a wide dataset keyed by timestamp of each input
# So will have multiple potential DVs as well as features (foods) wide

