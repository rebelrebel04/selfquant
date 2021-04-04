library(tidyverse)
library(ixplor)
library(ixtra)

source("./data_dot_gov_api.R")


# READ IN FOOD SENSITIVITY RESULTS ####
filename <- "./everlywell_foodsens_2021.txt"
ev.0 <- readChar(filename, file.info(filename)$size)
ev.1 <- 
  ev.0 %>% 
  stringr::str_split(pattern = "score") %>% 
  unlist() %>% 
  trimws() %>% 
  gsub("^01 ", "", .) %>% 
  as_tibble() %>%
  mutate(
    food = str_extract(value, "[A-Z]+[ ,]?[A-Z]+"),
    score = as.integer(trimws(str_extract(value, " \\d+ "))),
    result = str_extract(value, "(AB)?NORMAL"),
    category = case_when(
      score >= 116 ~ "High",
      score >= 58 ~ "Moderate",
      score >= 17 ~ "Mild",
      TRUE ~ "Normal"
    )
  ) %>% 
  filter(!is.na(food))

# Confirm parsing worked
cpf(ev.1, food %=~% "^(\\w|\\s|,)+$")
cpf(ev.1, result)
cpf(ev.1, category)
ss(ev.1, score)

# write_csv(ev.1, "./data/everlywell_foodsens_2021.csv")
# ev.1 <- read_csv("./data/everlywell_foodsens_2021.csv")



# SEARCH FDC DB ####

# keywords <- 
#   c(
#     "+macadamia +dry +salted",
#     "+wild +planet +sockeye +salmon"
#   )

food_search.0 <- 
  ev.1$food %>% 
  purrr::map_dfr(
    ~ GET_foods_search(.x, page_size = 5) %>% 
      parse_search_foods_df()
  )

food_search.0 %>% 
  select(!where(is.list)) %>% 
  write_csv("./data/food_search.0.csv")

# Read back in after manually scanning
food_search.1 <- 
  read_csv("./data/food_search.1.csv") %>% 
  group_by(keyword) %>% 
  mutate(flag_keep = max(flag_keep)) %>% 
  ungroup()

food_search.unmatched.kw <- 
  food_search.1 %>% 
  filter(flag_keep == 0) %>% 
  distinct(keyword_updated) %>% 
  filter(!is.na(keyword_updated))

# Search more aggressively for unmatched foods
#///TODO: GET_ funs need to handle empty return (at least, when search fun finds no matches)
food_search.unmatched.0 <- 
  food_search.unmatched.kw$keyword_updated %>% 
  purrr::map_dfr(
    ~ GET_foods_search(.x, page_size = 20) %>% 
      parse_search_foods_df()
  )



# LOOK UP NUTRIENTS FOR SELECTED FOODS ####
foods.0 <- 
  GET_foods(food_search$fdcId) %>% 
  parse_foods_df()