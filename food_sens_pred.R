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
    keyword = trimws(gsub("(^| )", " +", food)) %&% " -BABYFOOD -CANDIES -RESTAURANT",
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



# SEARCH FDC DB - first pass ####
slowly_GET_foods_search <- slowly(GET_foods_search)
possibly_parse_search_foods_df <- possibly(parse_search_foods_df, tibble(failed = TRUE))

food_search.0 <- 
  ev.1 %>% 
  select(food, keyword) %>% 
  purrr::pmap_dfr(
    ~ slowly_GET_foods_search(..2, page_size = 10) %>% 
      possibly_parse_search_foods_df() %>% 
      mutate(ev_food = ..1)
  ) %>% 
  select(ev_food, everything())
freeze(food_search.0)
revive(food_search.0)

food_search.0 %>% 
  select(!where(is.list)) %>% 
  write_csv("./data/food_search.0.csv")

# Read back in after manually scanning
food_search.0_updated <- 
  read_csv("./data/food_search.0_updated.csv")

food_search.unmatched.kw <- 
  food_search.0_updated %>% 
  group_by(keyword) %>% 
  mutate(flag_keep = max(flag_keep)) %>% 
  ungroup() %>% 
  filter(flag_keep == 0) %>% 
  distinct(keyword_update, .keep_all = TRUE) %>% 
  filter(keyword_update != "0") %>% 
  select(ev_food, keyword_update)

# Search more aggressively for unmatched foods
food_search.unmatched.0 <- 
  food_search.unmatched.kw %>% 
  purrr::pmap_dfr(
    ~ GET_foods_search(..2, page_size = 20) %>% 
      possibly_parse_search_foods_df() %>% 
      mutate(ev_food = ..1, kw = ..2) %>% 
      select(ev_food, kw, everything())
  )
freeze(food_search.unmatched.0)
revive(food_search.unmatched.0)

food_search.unmatched.0 %>% 
  select(!where(is.list)) %>% 
  write_csv("./data/food_search.unmatched.0.csv")

# Read back in after manually scanning
food_search.unmatched.1 <- 
  read_csv("./data/food_search.unmatched.1.csv") %>% 
  filter(flag_keep == 1)
dup(food_search.unmatched.1, keyword)

# Ok, close enough. Combine the two food lists
food_search.2 <- 
  food_search.0_updated %>% 
  filter(flag_keep == 1) %>% 
  bind_rows(food_search.unmatched.1) %>% 
  select(fdcId, ev_food, description) %>% 
  arrange(ev_food)
  
dup(food_search.2, ev_food)
has(food_search.2, fdcId)



# LOOK UP NUTRIENTS FOR SELECTED FOODS ####
possibly_parse_foods_df <- possibly(parse_foods_df, tibble())

foods.0 <- 
  food_search.2 %>% 
  pmap_dfr(
    ~ GET_foods(..1) %>% 
      possibly_parse_foods_df() %>% 
      mutate(ev_food = ..2) %>% 
      select(ev_food, everything())
  )
freeze(foods.0)
revive(foods.0)

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

# Any foods not returned?
setdiff(food_search.2$ev_food, foods.0$ev_food)
#///TODO: Figure out why these 39 foods aren't parsing... :(


# Spread nutrients wide
foods.1 <- 
  foods.0 %>% 
  distinct(fdcId, ev_food, nutrient, amount, .keep_all = TRUE) %>% 
  unite(nutrient, nutrient, unitName) %>% 
  pivot_wider(id_cols = c(fdcId, ev_food, food, category), names_from = nutrient, values_from = amount, values_fn = first, values_fill = 0) %>% 
  left_join(
    ev.1 %>% select(ev_food = food, ev_score = score, ev_category = category)
  )
freeze(foods.1)



na_cols <- 
  foods.1 %>% 
  map_lgl(~ all(is.na(.x))) %>% 
  which()
na_cols

nzv_cols <- 
  foods.1 %>% 
  map_lgl(~ var(.x, na.rm = TRUE) == 0) %>% 
  # map_lgl(~ min(.x, na.rm = TRUE) == max(.x, na.rm = TRUE)) %>%   
  which()
nzv_cols


foods.2 <- 
  foods.1 %>% 
  select(-all_of(c(na_cols, nzv_cols)), -`Vitamins and Other Components_g`) %>% 
  mutate(
    fdcId = paste(fdcId),
    across(where(is.numeric), ~ .x / Energy_kcal)
  ) %>% 
  select(-Energy_kcal, -Energy_kJ) %>% 
  filter(!is.infinite(Water_g)) %>% 
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.x)))
  )
names(foods.2) <- make.names(names(foods.2))

pca.fit <- prcomp(~ . -fdcId -ev_food -ev_score -ev_category -food -category, data = foods.2, scale = FALSE)
biplot(pca.fit)
screeplot(pca.fit)

# PC1 Loadings -- carb vs. fat?
data.frame(loading = pca.fit$rotation[, 1]) %>% 
  rownames_to_column("nutrient") %>% 
  arrange(loading) %>% 
  #head()
  ggplot(aes(x = reorder(nutrient, loading), y = loading)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw()

# PC2 Loadings -- fat vs. vitamins/minerals?
data.frame(loading = pca.fit$rotation[, 2]) %>% 
  rownames_to_column("nutrient") %>% 
  arrange(loading) %>% 
  #head()
  ggplot(aes(x = reorder(nutrient, loading), y = loading)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw()


lm.fit <- lm(ev_score ~ . -fdcId -ev_food -ev_category -food -category, data = foods.2)
summary(lm.fit)
broom::tidy(lm.fit) %>% 
  arrange(p.value)

pls.fit <- pls::plsr(ev_score ~ . -fdcId -ev_food -ev_category -food -category, data = foods.2)
summary(pls.fit)
plot(pls.fit)
biplot(pls.fit)
pls::coefplot(pls.fit)
pls::corrplot(pls.fit)
pls::explvar(pls.fit)
pls::loadingplot(pls.fit, identify = TRUE)
pls::scoreplot(pls.fit)
