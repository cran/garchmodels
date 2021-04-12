## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  
  out.width='100%',
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(garchmodels)
library(timetk)
library(tidyverse)
library(tidymodels)

## -----------------------------------------------------------------------------
rIBM

## -----------------------------------------------------------------------------
rIBM_extended <- rIBM %>%
    future_frame(.length_out = 3, .bind_data = TRUE) 

rIBM_train  <- rIBM_extended %>% drop_na()
rIBM_future <- rIBM_extended %>% filter(is.na(daily_returns))

## -----------------------------------------------------------------------------
# Model Spec
model_spec <-garchmodels::garch_reg(mode = "regression",
                                    arch_order = tune(),
                                    garch_order = tune(),
                                    tune_by = "sigmaFor") %>%
    set_engine("rugarch") 

## -----------------------------------------------------------------------------
# Recipe spec
recipe_spec <- recipe(daily_returns ~ date, data = rIBM_train) 

## -----------------------------------------------------------------------------
# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

## -----------------------------------------------------------------------------
resamples <- timetk::time_series_cv(rIBM_train, 
                                    date_var = date, 
                                    initial = "6 years", 
                                    assess = "24 months",
                                    skip = "24 months",
                                    cumulative = TRUE,
                                    slice_limit = 3)

timetk::plot_time_series_cv_plan(resamples, .date_var = date, .value = daily_returns)

## -----------------------------------------------------------------------------
tune_results <- tune_grid(
    object     = wflw,
    resamples  = resamples,
    param_info = parameters(wflw),
    grid       = 5,
    control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = "everything")
)

## -----------------------------------------------------------------------------
tune_results %>% show_best(metric = "rmse")

