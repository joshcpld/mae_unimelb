# Generate all of the different possible ARMA(p,q) parameter combinations

ARMA_params <- tibble(AR_param = rep(1:13, each = 4)) %>% 
  mutate(MA_params = rep(1:4, each = 13))


# Create a function which will generate all of the ARMA models for a given set of parameter combinations

estimate_ARMA_models <- function(data, params_df) {
  models <- map2(params_df$AR_param, params_df$MA_params, function(AR_param, MA_param) {
    Arima(data, order = c(AR_param, 0, MA_param), include.mean = FALSE, method = "ML")
  })
  
  return(models)
}


# Actually generate the ARMA models for the chosen set of parameter combinations

ARMA_models <- estimate_ARMA_models(data_decomposed$cyclical, ARMA_params)


# Create a function that will extract all the AIC and BIC values from a list of ARMA models

extract_ARMA_info <- function(model_output) {
  AR_order <- model_output$arma[1]
  MA_order <- model_output$arma[2]
  aic <- AIC(model_output)
  bic <- BIC(model_output)
  return(data.frame(AR_order = AR_order, MA_order = MA_order, AIC = aic, BIC = bic))
}


# Create a dataframe showing all of these AIC/BIC values and the model they match to.

ARMA_model_results <- map_dfr(ARMA_models, extract_ARMA_info)

# Test to see if AIC and BIC agree on which is the best model

ARMA_model_results_AIC <- ARMA_model_results %>% 
  select(AR_order, MA_order, AIC) %>% 
  arrange(AIC) %>% 
  print()

ARMA_model_results_BIC <- ARMA_model_results %>% 
  select(AR_order, MA_order, BIC) %>% 
  arrange(BIC) %>% 
  print()

# AIC and BIC both agree that ARMA(12,3) is the optimal model according

best_model <- Arima(data_decomposed$cyclical, 
                    order = c(12,0,3),
                    include.mean = FALSE,
                    method = "ML")

# Now we need to test 
