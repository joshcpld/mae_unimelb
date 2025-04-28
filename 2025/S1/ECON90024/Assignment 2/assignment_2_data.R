library(tidyverse)
library(tidyquant)
library(stringr)
library(readrba)

asx_tickers <- c(
  "BHP.AX",  # BHP Group
  "CBA.AX",  # Commonwealth Bank of Australia
  "RIO.AX",  # Rio Tinto
  "CSL.AX",  # CSL Limited
  "NAB.AX",  # National Australia Bank
  "WBC.AX",  # Westpac
  "ANZ.AX",  # ANZ Bank
  "WES.AX",  # Wesfarmers
  "MQG.AX",  # Macquarie Group
  "WOW.AX",  # Woolworths
  "TLS.AX",  # Telstra
  "FMG.AX",  # Fortescue Metals
  "^AXJO"   # ASX 200
)


raw_data <- tq_get(asx_tickers, 
                     get = "stock.prices", 
                     from = "2014-12-31",
                     to = "2024-12-31")


data <- raw_data %>%
  select(name = symbol, date, price = close) %>% 
  mutate(name = str_remove(name, "\\.AX$")) %>% 
  mutate(name = str_replace_all(name, "\\^", "")) %>% 
  #Creating monthly price series (using final trading day of month)
  mutate(month = format(date, "%Y-%m")) %>%  
  group_by(name, month) %>%  
  summarise(price = last(price)) %>%
  ungroup() %>% 
  # Creating returns
  arrange(name, month) %>% 
  group_by(name) %>% 
  mutate(return = (price / lag(price)* 100 - 100)) %>% 
  ungroup() %>%  
  select(name, date = month, price, return) %>% 
  na.omit() 

# RBA Data

rba_data <- read_rba_seriesid("FIRMMBAB90") %>% 
  select(date,value) %>% 
  mutate(monthly_rate = (1 + value / 100)^(1/12) - 1) %>% 
  mutate(month = format(date, "%Y-%m")) %>% 
  select(date = month, rf_rate = monthly_rate)


# Connecting data sources

data <- data %>% 
  inner_join(rba_data, by = "date")


