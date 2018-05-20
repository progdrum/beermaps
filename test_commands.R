library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(htmltools)


base_url <- "http://beermapping.com/webservice/"
api_key <- ""

# Call the damn API!
co_breweries <- GET(paste0(base_url, "locstate/", api_key, "/co&s=json"))
brew_contents <- content(co_breweries)

co_brewery_df <- map(brew_contents, function(x) {
  df <- tibble(id = x$id,
               name = x$name,
               type = x$status,
               review_link = x$reviewlink,
               proxy_link = x$proxylink,
               blog_map = x$blogmap,
               street = x$street,
               city = x$city,
               state = x$state,
               zip = x$zip,
               country = x$country,
               phone = x$phone,
               url = x$url,
               overall = x$overall,
               image_count = x$imagecount)
}) %>% bind_rows()

get_brewery_locs <- function(brewery_id) {
  loc <- content(GET(paste0(base_url,
                            "locmap/",
                            api_key,
                            "/",
                            brewery_id,
                            "&s=json")))
  loc[[1]]$id <- brewery_id
  return(loc)
}

brewery_locs <- map(co_brewery_df$id, get_brewery_locs)

brewery_loc_df <- map(brewery_locs, function(x) {
  brewery_loc <- x[[1]]
  df <- tibble(id = brewery_loc$id,
               name = ifelse(!is.null(brewery_loc$name), brewery_loc$name, NA),
               type = ifelse(!is.null(brewery_loc$status), brewery_loc$status, NA),
               latitude = ifelse(!is.null(brewery_loc$lat), brewery_loc$lat, NA),
               longitude = ifelse(!is.null(brewery_loc$lat), brewery_loc$lng, NA),
               map = ifelse(!is.null(brewery_loc$map), brewery_loc$map, NA),
               altmap = ifelse(!is.null(brewery_loc$altmap), brewery_loc$altmap, NA))
}) %>% bind_rows()

full_df <- 
  brewery_loc_df %>% 
  select(id, latitude, longitude) %>% 
  inner_join(co_brewery_df, by = "id")

full_df <- 
  full_df %>% 
  mutate_at(vars(longitude), funs(as.numeric)) %>% 
  mutate_at(vars(latitude), funs(as.numeric))

full_df %>% 
  filter(longitude != 0 & latitude != 0) %>% 
  leaflet() %>% 
  setView(lng = -105, lat = 39.5, zoom = 8) %>% 
  addTiles() %>% 
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             popup = ~paste(sep = "<br/ >",
                            name,
                            street,
                            paste(city, ", ", state, zip),
                            phone,
                            url))
