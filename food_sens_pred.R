library(tidyverse)
library(ixplor)
library(ixtra)

source("./data_dot_gov_api.R")
options(freezer = "./rds/")


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
    food = gsub(",", " ", food, fixed = TRUE),
    food = trimws(gsub("(^| )", " +", food)) %&% " -BABYFOOD",
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
slowly_GET_foods_search <- slowly(GET_foods_search)
possibly_parse_search_foods_df <- possibly(parse_search_foods_df, tibble())

# keywords <- 
#   c(
#     "+macadamia +dry +salted",
#     "+wild +planet +sockeye +salmon"
#   )

food_search.0 <- 
  ev.1$food %>% 
  purrr::map_dfr(
    ~ slowly_GET_foods_search(.x, page_size = 10) %>% 
      possibly_parse_search_foods_df()
  )
freeze(food_search.0)
revive(food_search.0)

food_search.0 %>% 
  select(!where(is.list)) %>% 
  write_csv("./data/food_search.0.csv")

# Read back in after manually scanning
food_search.1 <- 
  read_csv("./data/food_search.1.csv")

food_search.unmatched.kw <- 
  food_search.1 %>% 
  group_by(keyword) %>% 
  mutate(flag_keep = max(flag_keep)) %>% 
  ungroup() %>% 
  filter(flag_keep == 0) %>% 
  distinct(keyword_updated) %>% 
  filter(!is.na(keyword_updated))

# Search more aggressively for unmatched foods
food_search.unmatched.0 <- 
  food_search.unmatched.kw$keyword_updated %>% 
  purrr::map_dfr(
    ~ GET_foods_search(.x, page_size = 20) %>% 
      possibly_parse_search_foods_df()
  )
freeze(food_search.unmatched.0)

food_search.unmatched.0 %>% 
  select(!where(is.list)) %>% 
  write_csv("./data/food_search.unmatched.0.csv")

# Read back in after manually scanning
food_search.unmatched.1 <- 
  read_csv("./data/food_search.unmatched.1.csv") %>% 
  filter(flag_keep == 1)
dup(food_search.unmatched.1, keyword)

# Join the original keywords (from everlywell) back on
food_search.unmatched.2 <- 
  food_search.unmatched.1 %>% 
  rename(keyword_updated = keyword) %>% 
  left_join(
    food_search.1 %>% 
      filter(!is.na(keyword_updated)) %>% 
      distinct(keyword_updated, keyword)
  )
has(food_search.unmatched.2, keyword)

# Ok, close enough. Combine the two food lists
food_search.2 <- 
  food_search.1 %>% 
  filter(flag_keep == 1) %>% 
  bind_rows(
    food_search.unmatched.2
  ) %>% 
  select(-flag_keep, -keyword_updated)
  
dup(food_search.2, keyword)
has(food_search.2, keyword, fdcId)



# LOOK UP NUTRIENTS FOR SELECTED FOODS ####

possibly_parse_foods_df <- possibly(parse_foods_df, tibble())

test <- 
  food_search.2[1:5, ] %>% 
  pull(fdcId) %>% 
  GET_foods()

View(test$content)


foods.0 <- 
  food_search.2$fdcId %>% 
  #head(5) %>% 
  map_dfr(
    ~ GET_foods(.x) %>% 
      #parse_foods_df()
      possibly_parse_foods_df()
  )
freeze(foods.0)

# NEXT:
# Step through parse_foods_df() to see why some fields aren't
# coming through for many of the foods -- e.g., servingSize??


# Any foods not returned?
setdiff(food_search.2$fdcId, foods.0$fdcId)



# # Page by 20 foods (fdcId values) at a time
# page_starts <- seq(from = 1, to = nrow(food_search.2), by = 20)
# page_ends <- page_starts + 19
# page_ends[length(page_ends)] <- min(page_ends[length(page_ends)], nrow(food_search.2))
# page_starts; page_ends
# foods.0 <- 
#   tibble(page_starts, page_ends) %>% 
#   pmap_dfr(
#     ~ GET_foods(food_search.2$fdcId[..1:..2]) %>% 
#       possibly_parse_foods_df()
#   )

