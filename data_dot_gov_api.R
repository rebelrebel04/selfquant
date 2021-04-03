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


get_user_agent <- function() {
  httr::user_agent("fdc_api_R")
}


make_GET_request <- function(endpoint, timeout = 120) {

  cli::cli_alert_info("Submitting FDC API query...\n{crayon::blue(endpoint)}")
  
  t0 <- Sys.time()
  
  rp <- callr::r_bg(
    f = function(endpoint, timeout, ua) {
      httr::GET(
        endpoint,
        ua,
        httr::add_headers(Accept = "application/json"),
        httr::timeout(timeout)
      )  
    },
    args = list(endpoint, timeout, get_user_agent())
  )

  spinner <- cli::make_spinner("dots12")
  while (rp$is_alive()) {
    spinner$spin()
    Sys.sleep(.1)
  }
  
  spinner$finish()
  t1 <- Sys.time()
  resp <- rp$get_result()
  
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
  
  cli::cli_alert_success("Query succeeded in {format(difftime(t1, t0))}")
  
  list(
    content = parsed,
    url = endpoint,
    response = resp
  )
  
}



# https://themockup.blog/posts/2020-05-22-parsing-json-in-r-with-jsonlite/
# https://tidyr.tidyverse.org/articles/rectangle.html
#TODO: /foods endpoint handles up to 20 foods at a time
GET_foods <- function(fdc_ids, ...) {
  
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
  
  make_GET_request(endpoint, ...)
  
}
fdc_ids <- c("534358", "373052")
resp <- GET_foods(fdc_ids)
# str(resp$content, max.level = 1)




parse_foods_df <- function(resp) {
  
  # Create initial df with one row for each food (fdcId) 
  foods.0 <- 
    tibble(food = resp$content) %>% 
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
View(parse_foods_df(resp))



#TODO: get & parse funs for search/ endpoint
# curl -X GET "https://api.nal.usda.gov/fdc/v1/foods/search?query=macadamia&dataType=Branded,Foundation,Survey%20%28FNDDS%29,SR%20Legacy&pageSize=25&pageNumber=1&sortBy=fdcId&sortOrder=asc&api_key=1A6fLK9yfowSG7iq7LtmMI1XRMdZejbDPVqQIDJG" -H  "accept: application/json"

GET_foods_search <- function(keyword, page_size = 25,...) {
  
  # https://app.swaggerhub.com/apis/fdcnal/food-data_central_api/1.0.0#/FDC/getFoodsSearch
  # curl -X GET "https://api.nal.usda.gov/fdc/v1/foods/search?query=macadamia&dataType=Branded,Foundation,Survey%20%28FNDDS%29,SR%20Legacy&pageSize=25&pageNumber=1&sortBy=fdcId&sortOrder=asc&api_key=1A6fLK9yfowSG7iq7LtmMI1XRMdZejbDPVqQIDJG" -H  "accept: application/json"
  endpoint <- 
    "https://api.nal.usda.gov/fdc/v1/foods/search" %>% 
    httr::modify_url(
      query = list(
        query = keyword,
        dataType = "Branded,Foundation,Survey (FNDDS),SR Legacy",
        pageSize = page_size, #200 max
        pageNumber = 1,
        sortBy = "fdcId",
        sortOrder = "asc",
        api_key = get_api_key()
      )
    )
  
  make_GET_request(endpoint, ...)
  
}
# Search operators: https://fdc.nal.usda.gov/help.html#bkmk-2
keyword <- "+macadamia +dry +salted"
resp <- search_foods(keyword)


parse_search_foods_df <- function(resp) {
  
  # Create initial df with one row for each food (fdcId) 
  foods.0 <- 
    tibble(food = pluck(resp, "content", "foods")) %>% 
    unnest_wider(food) %>% 
    # score: Relative score indicating how well the food matches the search criteria.
    arrange(desc(score))
  
  foods.0
  
}
parse_search_foods_df(resp)


# Example ####
keywords <- 
  c(
    "+macadamia +dry +salted",
    "+wild +planet +sockeye +salmon"
  )

food_search <- 
  keywords %>% 
  purrr::map_dfr(~ GET_foods_search(.x, page_size = 1) %>% parse_search_foods_df())

foods.0 <- 
  GET_foods(food_search$fdcId) %>% 
  parse_foods_df()
  
