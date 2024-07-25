library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
        
        
setwd("C:/Users/joshc/OneDrive/Desktop/git/mae_unimelb/ECON90010 - QAFII/")

SP500 <- read_excel("SP500.xlsx") %>% 
  clean_names() %>%  
  select(Date = date, SP500 = last_price)

DAX <- read_excel("DAX.xlsx")

DJI <- read_excel("DJI.xlsx")

FTSE <- read_excel("FTSE.xlsx")

HSI <- read_excel("HSI.xlsx")

NKI <- read_excel("NKI.xlsx")

MXWO <- read_excel("MXWO.xlsx")

VIX <- read_excel("VIX.xlsx")

TBILL <- read_csv("TBILL.csv") %>% 
  select(Date = DATE, TBILL = DTB3) %>% 
  mutate_at(vars(TBILL), ~na_if(., "."))


data <- MXWO %>% 
  full_join(DAX, by = "Date") %>% 
  full_join(DJI, by = "Date") %>%
  full_join(FTSE, by = "Date") %>%
  full_join(HSI, by = "Date") %>%
  full_join(NKI, by = "Date") %>%
  full_join(SP500, by = "Date") %>%
  full_join(VIX, by = "Date") %>% 
  full_join(TBILL, by = "Date") %>% 
  select(Date, DJI, SP500, HSI, NKI, DAX, FTSE, MXWO, TBILL, VIX) %>% 
  filter(Date >= as.Date("1990-01-01")) %>% 
  mutate(Date = as.Date(Date))

data_no_nan <- data %>% 
  na.omit()


write_csv(data, "country_stocks.csv")

write_csv(data_no_nan, "country_stocks_no_nan.csv")
