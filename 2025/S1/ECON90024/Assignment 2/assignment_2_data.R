library(tidyverse)
library(tidyquant)
library(stringr)
library(readrba)
library(janitor)
library(broom)


################################################################################
############################### QUESTION 3A ####################################
################################################################################

# ASX Data #####################################################################

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
  select(name, date = month, value = return) %>% 
  na.omit() %>%
  pivot_wider(names_from = name,
              values_from = value) %>% 
  clean_names()
  

# RBA Data #####################################################################

rba_data <- read_rba_seriesid("FIRMMBAB90") %>% 
  select(date,value) %>% 
  mutate(monthly_rate = (1 + value / 100)^(1/12) - 1) %>% 
  mutate(month = format(date, "%Y-%m")) %>% 
  select(date = month, rf_rate = monthly_rate)


# Connecting data sources ######################################################

data <- data %>% 
  inner_join(rba_data, by = "date")



################################################################################
############################# QUESTION 3B  #####################################
################################################################################



# Computing excess returns #####################################################

capm_data <- data %>% 
  mutate(market_excess = axjo - rf_rate) %>% 
  mutate(
    
    across(
      .cols = -c(date, axjo, rf_rate, market_excess),
      .fn = ~.x - rf_rate,
      .names = "{.col}_excess"
    )
  ) %>% 
  select(date, matches("_excess$"))


# Producing CAPM regression results ############################################

# Extracting column names

excess_columns <- colnames(capm_data)[grepl("_excess$", colnames(capm_data))]

# Fit CAPM models and extract the beta coefficients

betas <- map(excess_columns, function(col) {
  model <- lm(as.formula(paste(col, "~ market_excess")), data = capm_data)
  coef(model)["market_excess"]
})

# Convert the list of betas to a dataframe

betas_df <- data.frame(name = excess_columns, beta = unlist(betas)) %>% 
  mutate(beta = round(beta,3)) %>% 
  mutate(name = str_remove(name, "_excess"))
print(betas_df)

# Write to csv for inclusion in report

write_csv(betas_df, "capm_betas_df.csv")


  



################################################################################
############################# QUESTION 3C ######################################
################################################################################


##################################### i ########################################

# Need to first calculate average returns



avg_returns <- data %>% 
  pivot_longer(cols = -date,
               names_to = "name",
               values_to = "value") %>% 
  group_by(name) %>% 
  summarise(avg_returns = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(name != "axjo")

print(avg_returns)

# Average rf return

avg_rf_return <- avg_returns %>% 
  filter(name == "rf_rate") %>% 
  pull(avg_returns)


# Now excess returns

excess_returns <- avg_returns %>% 
  filter(name != "rf_rate") %>% 
  mutate(rf_rate = avg_rf_return) %>% 
  mutate(excess_returns = avg_returns - rf_rate) %>% 
  select(name, excess_returns)



# Match with earlier returns

excess_regression_data <- excess_returns %>% 
  inner_join(betas_df, by = "name") 


# Produce regression

excess_returns_cross_section_regression <- lm(excess_returns ~ beta, data = excess_regression_data)

summary(excess_returns_cross_section_regression)

excess_returns_cross_section_regression_tidy <- excess_returns_cross_section_regression %>% 
  tidy()

write_csv(avg_returns_regression_tidy, "avg_returns_regression.csv")


# Producing average excess returns

overall_avg_returns <- excess_returns %>% 
  summarise(average_excess_returns = mean(excess_returns))
  


##################################### ii #######################################


p <- ggplot(excess_regression_data, aes(beta,excess_returns)) + 
  geom_point() +
  geom_abline(intercept = 0.7686, slope = -0.0562, color = "red", linetype = "dashed", size = 1) +
  annotate(
    "text",
    x = 0,  # adjust position as needed
    y = 1,
    label = "Excess returns = 0.77 - 0.06*Beta",
    hjust = 0,
    vjust = 1,
    color = "red",
    size = 4
  ) +
  labs(
        title = "Relationship between excess-returns and CAPM beta",
        caption = "Note: The red dotted line reflect the estimated regression line from Q3Cii."
        ) +
  theme(plot.caption = element_text(hjust = 0))

ggsave("q3cii.jpg", plot = p)

##################################### iii ######################################



coefficients <- coef(avg_returns_regression)

beta <- as.numeric(coef(avg_returns_regression)["beta"])
se <- summary(avg_returns_regression)$coefficients["beta", "Std. Error"]

t_stat <- beta / se

df <- df.residual(avg_returns_regression)

p_value <- pt(t_stat, df = df, lower.tail = TRUE)

print(p_value)
