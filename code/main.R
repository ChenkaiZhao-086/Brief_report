library(tidyverse)

source("fun.R")
dat <- read.csv("owid-covid-data.csv")

dat2 <- dat %>%
  dplyr::select(iso_code, continent, location, date, new_cases, new_deaths) %>%
  filter(location %in% c("Hong Kong", "Singapore", "Japan", "South Korea", "United Kingdom", "Australia", "United States", "South Africa"),
         date >= Sys.Date() - days(365)) %>%
  mutate(wek = week(date)) %>%
  group_by(location, wek) %>% 
  reframe(location = location,
          date = as.Date(date),
          case = sum(new_cases, na.rm = T),
          death = sum(new_deaths, na.rm = T)) %>% 
  distinct(location, wek, case, death, .keep_all = T) %>% 
  mutate(lable = paste0(year(date), "-W", wek))





fig(dat2, "Japan", type = "case")

# c("HKG", "SGP", "JPN", "KOR", "GBR", "AUS", "USA", "ZAF")

