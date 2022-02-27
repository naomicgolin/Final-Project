library(readr)
library(dplyr)
ufo_data <- read_csv("nuforc_reports.csv")

ufo_data <- ufo_data %>% 
  mutate("Year" = year(date_time),
         "Month" = as.factor(month(date_time)),
         "Date" = format(date_time, "%Y-%m-%d"))

ufo_data <- ufo_data[,c(1:4, 15, 13, 14, 5:12)]

ufo_data <- ufo_data %>% 
  dplyr::select(-c(stats, posted))


ufo_data <- ufo_data %>% 
  filter(state %in% state.abb)

dump("ufo_data", file="ufo_data.R")
rm(ufo_data)
source(file="ufo_data.R")

