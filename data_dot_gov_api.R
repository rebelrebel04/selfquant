library(dplyr)
library(tidyr)
library(purrr)


get_api_key <- function(env_var = "DATA_DOT_GOV_KEY") {
  
  key <- Sys.getenv(env_var)
  
  if (identical(key, "")) {
    stop(
      glue::glue("Please set env var {env_var} to your personal user key\nRequest a key for free at https://fdc.nal.usda.gov/api-key-signup.html"),
      call. = FALSE
    )
  }
  
  key
  
}


# https://themockup.blog/posts/2020-05-22-parsing-json-in-r-with-jsonlite/
# https://tidyr.tidyverse.org/articles/rectangle.html
#TODO: /foods endpoint handles up to 20 foods at a time
get_foods <- function(fdc_ids, timeout = 120) {
  
  # https://app.swaggerhub.com/apis/fdcnal/food-data_central_api/1.0.0#/FDC/getFoods
  # curl -X GET "https://api.nal.usda.gov/fdc/v1/foods?fdcIds=534358%2C373052&nutrients=203&api_key=1A6fLK9yfowSG7iq7LtmMI1XRMdZejbDPVqQIDJG" -H  "accept: application/json"  
  endpoint <- 
    "https://api.nal.usda.gov/fdc/v1/foods" %>% 
    httr::modify_url(
      query = list(
        fdcIds = paste(fdc_ids, collapse = ","),
        api_key = get_api_key()
      )
    )
  
  resp <-
    httr::GET(
      endpoint,
      get_user_agent(),
      httr::add_headers(Accept = "application/json"),
      httr::timeout(timeout)
    )
  
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- 
    jsonlite::fromJSON(
      httr::content(resp, "text"), 
      simplifyVector = FALSE
    )
  
  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "FDC API request failed [%s]\n%s\n<%s>", 
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  list(
    content = parsed,
    url = endpoint,
    response = resp
  )
  
}
# fdc_ids <- c("534358", "373052")
# resp <- get_foods(fdc_ids)
# str(resp$content, max.level = 1)




parse_foods_df <- function(resp) {
  
  # Create initial df with one row for each food (fdcId) 
  foods.0 <- 
    tibble(food = resp) %>% 
    unnest_wider(food)
  
  # Create a long df of nutrient info keyed by food (fdcId)
  nutrients.1 <- 
    foods.0 %>% 
    select(fdcId, foodNutrients) %>% 
    unnest_longer(foodNutrients) %>% 
    unnest_wider(foodNutrients) %>% 
    hoist(nutrient, "name", "unitName") %>% 
    hoist(foodNutrientDerivation, "description") %>% 
    select(fdcId, nutrient = name, amount, unitName, description)

  # Join the long nutrient info onto food-level df
  # This intentionally duplicates food-level variables
  foods.1 <-
    foods.0 %>%
    select(fdcId, food = description, ingredients, servingSize, servingSizeUnit) %>% 
    left_join(nutrients.1, by = "fdcId")
  
  foods.1
  
}
# parse_foods_df(resp)



#TODO: get & parse funs for search/ endpoint


