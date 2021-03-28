#ev.0 <- readr::read_delim("~/Desktop/everlywell.txt", delim = " ", col_names = FALSE)
library(tidyverse)

fileName <- "./everlywell_foodsens_2021.txt"
ev.0 <- readChar(fileName, file.info(fileName)$size)
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
  )

write_csv(ev.1, "./data/everlywell_foodsens_2021.csv")
