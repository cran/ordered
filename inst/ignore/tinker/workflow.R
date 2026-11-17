# This script is for development and is not included in the package.
# It demonstrates the use of the `threshold_structure` argument with the
# `clm` and `vglm` engines.

library(parsnip)
library(ordered)
library(workflows)
library(tune)
library(dials)
library(MASS)
library(VGAM)

set.seed(144688L)
house_data <- MASS::housing
house_sub <-
  house_data[sample(72, 120, replace = TRUE, prob = house_data$Freq), ] |>
  subset(select = -Freq)
train_inds <- sample(120, 80)
house_train <- house_sub[train_inds, ]
house_test <- house_sub[-train_inds, ]

# -------------------------------------------------------------------------
# clm engine with threshold_structure
# -------------------------------------------------------------------------

clm_spec <- ordinal_reg(
  ordinal_link = "logistic",
  threshold_structure = "symmetric_central"
) |>
  set_engine("clm") |>
  set_mode("classification")

clm_wf <- workflow() |>
  add_variables(outcomes = Sat, predictors = everything()) |>
  add_model(clm_spec)

clm_fit <- fit(clm_wf, data = house_train)
predict(clm_fit, house_test)
predict(clm_fit, house_test, type = "prob")

# -------------------------------------------------------------------------
# clm engine with tuning (uncomment to test)
# -------------------------------------------------------------------------

# clm_spec_tune <- ordinal_reg(
#   ordinal_link = "logistic",
#   threshold_structure = tune()
# ) |>
#   set_engine("clm") |>
#   set_mode("classification")
# 
# clm_wf_tune <- workflow() |>
#   add_variables(outcomes = Sat, predictors = everything()) |>
#   add_model(clm_spec_tune)
# 
# # This will error because `grid_regular()` for `threshold_structure()` requires
# # the threshold_structure dial to be accessible via `dials::threshold_structure`.
# # TODO: Debug the tuning workflow once dials integration is finalized.
# clm_grid <- grid_regular(
#   threshold_structure(),
#   levels = 2
# )

# -------------------------------------------------------------------------
# vglm engine with threshold_structure
# -------------------------------------------------------------------------

vglm_spec <- ordinal_reg(
  ordinal_link = "logistic",
  threshold_structure = "equidistant"
) |>
  set_engine("vglm") |>
  set_mode("classification")

vglm_wf <- workflow() |>
  add_variables(outcomes = Sat, predictors = everything()) |>
  add_model(vglm_spec)

vglm_fit <- fit(vglm_wf, data = house_train)
predict(vglm_fit, house_test)
predict(vglm_fit, house_test, type = "prob")

# -------------------------------------------------------------------------
# vglm engine with symmetric_central threshold (maps to VGAM's "symm1")
# -------------------------------------------------------------------------

vglm_spec_symm <- ordinal_reg(
  ordinal_link = "logistic",
  threshold_structure = "symmetric_central"
) |>
  set_engine("vglm") |>
  set_mode("classification")

vglm_wf_symm <- workflow() |>
  add_variables(outcomes = Sat, predictors = everything()) |>
  add_model(vglm_spec_symm)

vglm_fit_symm <- fit(vglm_wf_symm, data = house_train)
predict(vglm_fit_symm, house_test, type = "prob")

# -------------------------------------------------------------------------
# vglm engine with qnorm threshold (VGAM-specific extension)
# -------------------------------------------------------------------------

vglm_spec_qnorm <- ordinal_reg(
  ordinal_link = "logistic",
  threshold_structure = "qnorm"
) |>
  set_engine("vglm") |>
  set_mode("classification")

vglm_wf_qnorm <- workflow() |>
  add_variables(outcomes = Sat, predictors = everything()) |>
  add_model(vglm_spec_qnorm)

vglm_fit_qnorm <- fit(vglm_wf_qnorm, data = house_train)
predict(vglm_fit_qnorm, house_test, type = "prob")
