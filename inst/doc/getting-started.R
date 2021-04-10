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

## ---- echo=F,  out.width="100%", fig.align='center'---------------------------
knitr::include_graphics("univariate_multivariate.png")

## ----setup--------------------------------------------------------------------
library(garchmodels)
library(timetk)
library(tidymodels)
library(tidyverse)

## -----------------------------------------------------------------------------
rIBM

## -----------------------------------------------------------------------------
rIBM_extended <- rIBM %>%
    future_frame(.length_out = 3, .bind_data = TRUE) 

rIBM_train  <- rIBM_extended %>% drop_na()
rIBM_future <- rIBM_extended %>% filter(is.na(daily_returns))

## -----------------------------------------------------------------------------
model_fit_garch <-garchmodels::garch_reg(mode = "regression",
                                         arch_order = 1,
                                         garch_order = 1,
                                         ma_order = 0,
                                         ar_order = 0) %>%
                  set_engine("rugarch", mean.model = list(include.mean = FALSE)) %>%
                  fit(daily_returns ~ date, data = rIBM_train)

## -----------------------------------------------------------------------------
plot(model_fit_garch$fit$models$model_1, which = 2)

## -----------------------------------------------------------------------------
plot(model_fit_garch$fit$models$model_1, which = 3)

## -----------------------------------------------------------------------------
predict(model_fit_garch, rIBM_future)

## -----------------------------------------------------------------------------
model_fit_garch <-garchmodels::garch_reg(mode = "regression",
                                         arch_order = 1,
                                         garch_order = 1,
                                         ma_order = 2,
                                         ar_order = 2) %>%
                  set_engine("rugarch") %>%
                  fit(daily_returns ~ date, data = rIBM_train)


## -----------------------------------------------------------------------------
plot(model_fit_garch$fit$models$model_1, which = 2)

## -----------------------------------------------------------------------------
plot(model_fit_garch$fit$models$model_1, which = 3)

## -----------------------------------------------------------------------------
predict(model_fit_garch, rIBM_future)

## -----------------------------------------------------------------------------
rX_longer

## -----------------------------------------------------------------------------
rX_longer_extended <- rX_longer %>%
    group_by(id) %>%
    future_frame(.length_out = 3, .bind_data = TRUE) %>%
    ungroup()

rX_train  <- rX_longer_extended %>% drop_na()
rX_future <- rX_longer_extended %>% filter(is.na(value))

## -----------------------------------------------------------------------------
model_fit <- garch_multivariate_reg(mode = "regression", type = "ugarchspec") %>%
    set_engine("rugarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                        spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                        spec3 = list(mean.model = list(armaOrder = c(1,0)))),
    ) %>%
    fit(value ~ date + id, data = rX_train)

## -----------------------------------------------------------------------------
predict(model_fit, rX_future)

## -----------------------------------------------------------------------------
model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
    set_engine("dcc_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                            spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                            spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                               dcc_specs = list(dccOrder = c(2,2), distribution = "mvlaplace")) %>%
    fit(value ~ date + id, data = rX_train)

## -----------------------------------------------------------------------------
plot(model_fit$fit$models$model_1, which = 1)

## -----------------------------------------------------------------------------
plot(model_fit$fit$models$model_1, which = 2)

## -----------------------------------------------------------------------------
plot(model_fit$fit$models$model_1, which = 3)

## -----------------------------------------------------------------------------
plot(model_fit$fit$models$model_1, which = 4)

## -----------------------------------------------------------------------------
plot(model_fit$fit$models$model_1, which = 5)

## -----------------------------------------------------------------------------
predictions <- predict(model_fit, rX_future)

## -----------------------------------------------------------------------------
predictions %>% filter(.name == "R") %>% pull(.pred)

## -----------------------------------------------------------------------------
model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
    set_engine("c_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                          spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                          spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                             c_specs = list(dccOrder = c(2,2))) %>%
    fit(value ~ date + id, data = rX_train)

model_fit

## ---- warning=FALSE, message=FALSE--------------------------------------------
model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
         set_engine("gogarch_rmgarch" , gogarch_specs = list(variance.model = list(garchOrder = c(2,2)))) %>%
    fit(value ~ date + id, data = rX_train)

## -----------------------------------------------------------------------------
predictions <- predict(model_fit, rX_future)
predictions

## -----------------------------------------------------------------------------
predictions %>% filter(.name == "garchcoef") %>% pull(.pred)

