# Hypothesis 2: Time
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(sf)
library(maps)
library(stringr)
library(tidytext)
library(tidyverse)
library(rvest)
library(httr)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) 
library(grid)
library(viridisLite)
library(gifski)

source(file="ufo_data.R") # Created in file named "Data Cleaning.R"
source(file="state_population.R") # Created in file named "State Population df.R"

# Visualization
ufo_place <- ufo_data %>% 
  group_by(state) %>% 
  summarise("count" = n())
ufo_place <- merge(ufo_place, population_df[,c(1,7)], by = "state", all.y = TRUE)
ufo_place <- ufo_place %>% 
  mutate("percentage" = (count / mean) * 100)
ufo_place <- ufo_place %>% 
  mutate("per_100000" = 100000 * percentage)
us_states_new <- st_transform(us_states, 2163)
us_states_new[[2]] <- state.abb[match(us_states_new[[2]], state.name)]
colnames(us_states_new)[2] <- "state"
us_states_new <- us_states_new[-29,]

ufo_place <- merge(ufo_place, us_states_new[, c(2, 7)], by = "state", all.y = TRUE)
ufo_place <- st_as_sf(ufo_place)

# As percentage of US Population
ufo_place_perc <- tm_shape(ufo_place) +
  tm_polygons(col = "percentage", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sighting as Percentage of US Population") +
  tm_layout(main.title = "UFO Sightings as Percentage of the Total US Population (%)",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 2, frame = FALSE,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0, 0.5),
            legend.text.size = 0.6)
ufo_place_perc

# Per 100,000
ufo_place_100000 <- tm_shape(ufo_place) +
  tm_polygons(col = "per_100000", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sightings Per 100,000 People") +
  tm_layout(main.title = "Number UFO Sightings Per 100,000 People By State",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 2, frame = FALSE,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0, 0.5),
            legend.text.size = 0.6)
ufo_place_100000

# Statistical Test
## States were split into regions:

regions <- list(
  west = c("WA", "OR", "CA", "NV", "AZ", "ID", "MT", "WY",
           "CO", "NM", "UT", "AK", "HI"),
  south = c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY",
            "GA", "FL", "SC", "NC", "VA", "WV"),
  midwest = c("KS", "NE", "SD", "ND", "MN", "MO", "IA", "IL",
              "IN", "MI", "WI", "OH"),
  northeast = c("ME", "NH", "NY", "MA", "RI", "VT", "PA",
                "NJ", "CT", "DE", "MD")
)

ufo_data$regions <- sapply(ufo_data$state,
                           function(x) names(regions)[grep(x,regions)])

region_count <- ufo_data %>% na.omit() %>%
  group_by(regions) %>%
  summarise("count" = n())

(regions_test <- chisq.test(region_count$count, p = c(12/50, 11/50, 14/50, 13/50)))
regions_test$observed - regions_test$expected
### Midwest: 2732 less cases reported than expected
### Northeast: 2265 less cases reported than expected
### South: 547 more cases reported than expected
### West: 4450 more cases reported than expected
