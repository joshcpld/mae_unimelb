################################################################################
################################# SET-UP #######################################
################################################################################

library(tidyverse)
library(naniar)


################################################################################
############################## IMPORTING DATA ##################################
################################################################################


################################ MENTAL HEALTH TABLE ###########################

s10ai <- read_csv("data/S10AI.csv") %>% 
  select(hhno, hhmid, depressed, depression, s1d_1, s1d_2, s1d_4i) %>% 
  select(hhno, hhmid, kessler_score = depressed, depression_group = depression, sex = s1d_1,
         rship_to_hh_head = s1d_2, age = s1d_4i) %>% 
  mutate(depression_dummy = case_when(
    
    depression_group > 1 ~ 1, # Depressed
    TRUE ~ 0 # Not depressed
    
  )) 



################################ HOUSEHOLD TABLES ##############################

# Water table  

s12ai <- read_csv("data/S12AI.csv") %>% 
  select(hhno, 
         water_source = s12a_9i, # Main source of drinking water
         water_distance = s12a_10ai, # Distance of drinking water source
         water_distance_units = s12a_10aii # Distance unit
        )

# Electricity and sewerage table

s12aii <- read_csv("data/S12AII.csv") %>% 
  select(hhno,
         electricity = s12b_14, # Does your house have electricity?
         open_sewerage = s12b_23 # Is there any open sewer,drain in/around the house?
    
  )



################################################################################
################################ JOINING DATA ##################################
################################################################################

data <- s10ai %>% 
  left_join(s12ai, by = "hhno") %>% 
  left_join(s12aii, by = "hhno")



vis_miss(data)


################################################################################
############################### VISUALISING DATA ###############################
################################################################################

ggplot(data, aes(depression_dummy)) + geom_histogram()



