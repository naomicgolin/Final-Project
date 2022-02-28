# Hypothesis 1: Time
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

# Dataset
source(file="ufo_data.R") # Created in file named "Data Cleaning.R"

## Obtaining total population of the US
webpage_time <- "https://en.wikipedia.org/wiki/Demographics_of_the_United_States"

population_time <- webpage_time %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(75) td:nth-child(2)") %>% 
  html_text() 
population_time <- sapply(str_extract_all(population_time, "\\d"), function(x) as.numeric(paste0(x, collapse = "")))[35:85]
population_time <- data.frame("Year" = 1969:2019, population_time)


## Visualization
ufo_year <- ufo_data %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  summarise("count" = n())
ufo_year_per <- merge(x = ufo_year, y = population_time, by = "Year", all.x = TRUE) %>% 
  mutate(perc = (count / population_time)*100)

ggplot(data =ufo_year_per, aes(x = Year, y = perc)) + 
  geom_line(color = "#08519c") + 
  labs(title = "Total Number of UFO Sightings Per Year as a percentage of the total population of the United States",
       subtitle = "The trend suggests a significant increase in the number of sightings in the mid-2000s", x = "Year",
       y = "Total Number of UFO Sightings \n(As a Percentage of the US Population)",
       caption = "Notes: The data used in this analysis was from NUFORC and US Census") +
  theme_bw() + theme(text = element_text(family = "ArialMT"),
                     plot.title = element_text(size = rel(2)), 
                     axis.text.x = element_text(size = rel(1)),
                     axis.text.y = element_text(size = rel(1))) +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0),
        plot.margin = unit(c(2, 2, 2, 2), "cm")) 

## Chi-square goodness of fitness test
year_count <- ufo_data %>% na.omit() %>% 
  group_by(Year) %>% 
  summarise("count" = n())

chisq.test(year_count$count, p = rep(1/length(levels(as.factor(ufo_data$Year))),
                                     length(levels(as.factor(ufo_data$Year)))))

### We reject the null hypotehsis