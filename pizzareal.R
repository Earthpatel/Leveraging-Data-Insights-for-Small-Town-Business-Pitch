library(httr)
library(tidyverse)

# Define API endpoint and parameters
api_url <- "https://api.yelp.com/v3/businesses/search"
api_key <- 'xB_Oyyx8bTdrMiUucVoweB7GENxhRzhz5R-KL1wNamTEmsd5WH83w9WEqSn3fldYhRUk-ZPYp297WwzXyWZ4yjmGU7Bo1LvxcBEkkzRZVy85oJMu5gu6-iUzQ7XPZXYx'  # Replace this with your actual Yelp API key
search_term <- "pizza"  # Search for pizza restaurants

# List of cities or neighborhoods in Upstate South Carolina
upstate_sc_locations <- c("Greenville", "Spartanburg", "Anderson", "Greer", "Simpsonville", "Easley", "Clemson", "Travelers Rest", "Mauldin", "Taylors")

# Initialize lists to store restaurant information
restaurant_names <- c()
restaurant_ratings <- c()
restaurant_cities <- c()
restaurant_prices <- c()

# Make GET requests for each location with authentication
for (location in upstate_sc_locations) {
  response <- GET(
    url = api_url,
    query = list(term = search_term, location = location, limit = 50),
    add_headers("Authorization" = paste("Bearer", api_key))
  )
  
  # Check if request was successful
  if (http_status(response)$category == "Success") {
    # Parse JSON response
    yelp_data <- content(response, as = "parsed")
    
    # Extract information for each restaurant
    for (business in yelp_data$businesses) {
      restaurant_names <- c(restaurant_names, business$name)
      restaurant_ratings <- c(restaurant_ratings, business$rating)
      restaurant_cities <- c(restaurant_cities, business$location$city)
      if ("price" %in% names(business)) {
        restaurant_prices <- c(restaurant_prices, business$price)
      } else {
        restaurant_prices <- c(restaurant_prices, NA)
      }
    }
  } else {
    # Print error message
    cat("Error:", http_status(response)$reason, "\n")
  }
}

# Check lengths of lists
lengths <- c(names = length(restaurant_names), ratings = length(restaurant_ratings),
             cities = length(restaurant_cities), prices = length(restaurant_prices))

# Check if lengths are consistent
if (length(unique(lengths)) > 1) {
  cat("Error: Lengths of restaurant information lists are not consistent.\n")
} else {
  # Create dataframe from lists
  restaurants_df <- data.frame(Name = restaurant_names, Rating = restaurant_ratings, 
                               City = restaurant_cities, Price = restaurant_prices)
  
  # Print dataframe
  print(restaurants_df)
}


#how many pizza restaurants in each city
gree <- restaurants_df %>% filter(City=='Greenville')
gree1 <- restaurants_df %>% filter(City=='Spartanburg')

meanrating <- restaurants_df %>% filter(City == 'Greenville') %>% select(sa = Rating) %>% mutate(meann = mean(sa))
meanratingsp <- restaurants_df %>% filter(City == 'Spartanburg') %>% select(sa2 = Rating) %>% mutate(meann = mean(sa2))

greenville <- restaurants_df %>% filter(City %in% c('Greenville','Spartanburg')) %>% mutate(GreenvillePercap = 72095/165, SpartanburgPercap = 38401/28)

ggplot(greenville, aes(x = City, fill = City)) +  # Map 'City' to fill color
  geom_bar() +  # Bar plot
  labs(title = "Frequency of Pizza Restaurants in Greenville and Spartanburg", x = "City", y = "Count") +
  scale_fill_brewer(palette = "Set3")

ggplot()