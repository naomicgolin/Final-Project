# Hypothesis 3: Time and Place
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

ufo_decade <- ufo_data %>% na.omit() %>% 
  mutate(decade = ifelse(Year < 1980, "1970", 
                         ifelse(Year < 1990, "1980",
                                ifelse(Year < 2000, "1990",
                                       ifelse(Year < 2010, "2000", "2010")))))
  
ufo_decade <- ufo_decade %>% na.omit() %>% 
  group_by(state, decade) %>% 
  summarise("count" = n())
population_df_pivot <- pivot_longer(population_df,
             cols = `1970`:`2010`,
             names_to = "decade",
             values_to = "population")

ufo_decade <- merge(x = ufo_decade, y = population_df_pivot[,-2], by = c("state", "decade"),
      all.x = TRUE) %>% 
  mutate("percentage" = (count / population) * 100)
ufo_decade <- ufo_decade %>% 
  mutate("per_100000" = 100000 * percentage)
ufo_state_decade_map <- merge(ufo_decade, us_states_new[, c(2, 7)], by = "state", all.y = TRUE)
ufo_state_decade_map <- st_as_sf(ufo_state_decade_map)

ufo_state_decade_map_perc <- tm_shape(ufo_state_decade_map) +
  tm_polygons(col = "percentage", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sighting as Percentage of US Population") +
  tm_facets(by = "decade", ncol = 1) +
  tm_layout(legend.outside.size = 0.2, main.title = "UFO Sightings as Percentage of the Total US Population (%)",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 1,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0.05, 0.5),
            legend.text.size = 0.6)
ufo_state_decade_map_perc

ufo_state_decade_map_100000 <- tm_shape(ufo_state_decade_map) +
  tm_polygons(col = "per_100000", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sightings Per 100,000 People") +
  tm_facets(by = "decade", ncol = 1) +
  tm_layout(legend.outside.size = 0.2, main.title = "Number UFO Sightings Per 100,000 People By State",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 1,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0.05, 0.5),
            legend.text.size = 0.6)

# Grouping 1970s-1990s together
ufo_decade_combined <- ufo_data %>% na.omit() %>% 
  mutate(decade = ifelse(Year < 2000, "1969-2000",
                         ifelse(Year < 2010, "2000s", "2010s")))
ufo_decade_combined <- ufo_decade_combined %>% na.omit() %>% 
  group_by(state, decade) %>% 
  summarise("count" = n())
population_df_pivot_combined <- pivot_longer(population_df,
                                    cols = `1970`:`2010`,
                                    names_to = "decade",
                                    values_to = "population")
population_df_pivot_combined <- population_df_pivot_combined %>% 
  mutate(decade = ifelse(decade < 2000, "1969-2000",
                         ifelse(decade < 2010, "2000s", "2010s")))
ufo_decade_combined <- merge(x = ufo_decade_combined, y = population_df_pivot_combined[,-2], by = c("state", "decade"),
                    all.x = TRUE) %>% 
  mutate("percentage" = (count / population) * 100)
ufo_decade_combined <- ufo_decade_combined %>% 
  mutate("per_100000" = 100000 * percentage)
ufo_state_decade_combined_map <- merge(ufo_decade_combined, us_states_new[, c(2, 7)], by = "state", all.y = TRUE)
ufo_state_decade_combined_map <- st_as_sf(ufo_state_decade_combined_map)

ufo_state_decade_map_combined_perc <- tm_shape(ufo_state_decade_combined_map) +
  tm_polygons(col = "percentage", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sighting as Percentage of US Population") +
  tm_facets(by = "decade", ncol = 1) +
  tm_layout(legend.outside.size = 0.2, main.title = "UFO Sightings as Percentage of the Total US Population (%)",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 1,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0.01, 0.5),
            legend.text.size = 0.6)
ufo_state_decade_map_combined_perc

ufo_state_decade_map_combined_100000 <- tm_shape(ufo_state_decade_combined_map) +
  tm_polygons(col = "per_100000", border.col = "black", style = "cont", 
              pal = viridis(10, direction = -1),
              title = "Sightings Per 100,000 People") +
  tm_facets(by = "decade", ncol = 1) +
  tm_layout(legend.outside.size = 0.2, main.title = "Number UFO Sightings Per 100,000 People By State",
            legend.title.size  = 1, main.title.position = "centre", main.title.size = 1,
            legend.frame = "black", legend.outside = TRUE, legend.position = c(0.01, 0.5),
            legend.text.size = 0.6)
ufo_state_decade_map_combined_100000
  
# Statistical Test: Chi-square

interaction_count <- ufo_data %>% na.omit() %>%
  group_by(regions, Year) %>%
  tally() %>% spread(regions, n)


chisq.test(interaction_count)

# Professor suggestion: Correlation coefficient
