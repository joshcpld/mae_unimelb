library(tidyverse)
library(visdat)
library(skimr)

################################################################################
######################### EXTRACTING KESSLER SCORES ############################
################################################################################

s10ai <- read_csv("S10AI.csv") 

s10ai <- s10ai %>% 
  mutate_all(~ replace_na(.x,0)) %>% 
  unite("id", id1, id2, id3, id4, hhmid, sep = "")



################################################################################
######################## EXTRACTING LABOUR MARKET DATA #########################
################################################################################

s1ei <- read_csv("S1EI.csv") %>% 
  unite("id", id1, id2, id3, id4, hhmid, sep = "")


################################################################################
########################## LINKING DATA SOURCES ################################
################################################################################


data <- s10ai %>% 
  inner_join(s1ei, by = "id")


################################################################################
############################ SUMMARY STATISTICS ################################
################################################################################

data_clean <- data %>% 
  select(id, depressed, depression, s1ei_10i, s1ei_11) %>% 
  #s1ei_11 = over what period is payment receive/owed? (==3 is monthly)
  filter(s1ei_11 == 3) %>% 
  na.omit() %>% 
  filter(s1ei_10i != 48000)

vis_dat(data_clean)

data_summary <- data_clean %>% 
  skim() %>% 
  select(variable = skim_variable, character.n_unique, numeric.mean, numeric.sd,
         numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100,numeric.hist) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)))

ggplot(data_clean, aes(depressed, s1ei_10i)) + geom_jitter() + 
  labs(x = "Kessler score", y = "Monthly wages")
 
