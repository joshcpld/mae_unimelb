library(tidyverse)
library(lubridate)

setwd("C:/Users/joshc/OneDrive/Desktop/git/mae_unimelb/ECON90010 - QAFII/")

country_stocks <- read_csv("country_stocks.csv") %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  

TBILL <- read_csv("TBILL.csv") %>% 
  # For some reason there are a bunch of missing values. 
  #Some are US public holidays (Thanksgiving/ 4th July). But not sure what many others are.
  mutate_all(na_if, ".") %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))


country_stocks_augmented <- country_stocks %>% 
  full_join(MXWO, by = "Date") %>% 
  full_join(TBILL, by = "Date")


write_csv(country_stocks_augmented, "country_stocks_augmented.csv")


