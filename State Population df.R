# Obtaining Population Data
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

# 1970:
webpage_1970 <- "https://en.wikipedia.org/wiki/1970_United_States_census"

population_1970 <- webpage_1970 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(11) td:nth-child(3)") %>% 
  html_text() 
population_1970 <- population_1970[-length(population_1970)]

population_1970 <- as.numeric(gsub(",","",str_sub(population_1970, 1, nchar(population_1970)-1)))
state_1970 <- webpage_1970 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(11) td:nth-child(2)") %>% 
  html_text() 
state_1970 <- state_1970[-length(state_1970)]

state_1970 <- str_sub(state_1970, 2, nchar(state_1970)-1)
state_1970 <- state.abb[(match(state_1970, state.name))]
population_1970_df <- data.frame("state" = state_1970,
                                 "population" = population_1970)
population_1970_df <- population_1970_df %>% 
  na.omit()


# 1980:
webpage_1980 <- "https://en.wikipedia.org/wiki/1980_United_States_census"

population_1980 <- webpage_1980 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(17) td:nth-child(4)") %>% 
  html_text() 
population_1980 <- population_1980[-length(population_1980)]

population_1980 <- as.numeric(gsub(",","",str_sub(population_1980, 1, nchar(population_1980)-1)))

state_1980 <- webpage_1980 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(17) td:nth-child(2)") %>% 
  html_text() 
state_1980 <- state_1980[-length(state_1980)]

state_1980 <- str_sub(state_1980, 2, nchar(state_1980)-1)
state_1980 <- state.abb[(match(state_1980, state.name))]
population_1980_df <- data.frame("state" = state_1980,
                                 "population" = population_1980)
population_1980_df <- population_1980_df %>% 
  na.omit()

# 1990:
webpage_1990 <- "https://en.wikipedia.org/wiki/1990_United_States_census"

population_1990 <- webpage_1990 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(13) tr :nth-child(4)") %>% 
  html_text() 
population_1990 <- population_1990[-c(1,length(population_1990))]

population_1990 <- as.numeric(gsub(",","",str_sub(population_1990, 1, nchar(population_1990)-1)))

state_1990 <- webpage_1990 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(13) tr :nth-child(2)") %>% 
  html_text() 
state_1990 <- state_1990[-1]

state_1990 <- state_1990[seq(2,length(state_1990), by = 2)]
state_1990 <- state_1990[-length(state_1990)]

state_1990 <- state.abb[(match(state_1990, state.name))]
population_1990_df <- data.frame("state" = state_1990,
                                 "population" = population_1990)
population_1990_df <- population_1990_df %>% 
  na.omit()

# 2000s:
webpage_2000 <- "https://en.wikipedia.org/wiki/2000_United_States_census"

population_2000 <- webpage_2000 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(13) td:nth-child(4)") %>% 
  html_text() 
population_2000 <- population_2000[-((length(population_2000) - 1): length(population_2000))]

population_2000 <- as.numeric(gsub(",","",str_sub(population_2000, 1, nchar(population_2000)-1)))

state_2000 <- webpage_2000 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(13) td:nth-child(2)") %>% 
  html_text() 
state_2000 <- state_2000[-((length(state_2000) - 1): length(state_2000))]

state_2000 <- str_sub(state_2000, 2, nchar(state_2000)-1)
state_2000 <- state.abb[(match(state_2000, state.name))]

population_2000_df <- data.frame("state" = state_2000,
                                 "population" = population_2000)
population_2000_df <- population_2000_df %>% 
  na.omit()

# 2010s:
webpage_2010 <- "https://en.wikipedia.org/wiki/2010_United_States_census"

population_2010 <- webpage_2010 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(53) td:nth-child(4)") %>% 
  html_text() 
population_2010 <- population_2010[-((length(population_2010) - 1): length(population_2010))]

population_2010 <- as.numeric(gsub(",","",str_sub(population_2010, 1, nchar(population_2010)-1)))

state_2010 <- webpage_2010 %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()  %>% 
  html_nodes(":nth-child(53) td:nth-child(2)") %>% 
  html_text() 
state_2010 <- state_2010[-((length(state_2010) - 1): length(state_2010))]

state_2010 <- str_sub(state_2010, 2, nchar(state_2010)-1)
state_2010 <- state.abb[(match(state_2010, state.name))]

population_2010_df <- data.frame("state" = state_2010,
                                 "population" = population_2010)
population_2010_df <- population_2010_df %>% 
  na.omit()

# Combining the populations into a df:
df_list <- list(population_1970_df, population_1980_df, population_1990_df,
                population_2000_df, population_2010_df)
population_df <- df_list %>% 
  reduce(full_join, by = 'state')
colnames(population_df) <- c("state", seq(1970, 2010, by = 10))

# Average population of each state from 1970-2010s:
population_df <- population_df %>% 
  group_by(state) %>% 
  mutate("mean" = mean(c(`1970`, `1980`, `1990`, `2000`, `2010`)))

dump("population_df", file="state_population.R")

