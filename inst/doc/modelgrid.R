## ---- include = FALSE----------------------------------------------------
library(magrittr)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- message = FALSE----------------------------------------------------
library(modelgrid)
mg <- model_grid()

mg

## ---- message = FALSE----------------------------------------------------
library(magrittr)
library(caret)
library(dplyr)
library(purrr)
# Load data on German credit applications.  
data(GermanCredit)

# Construct empty model grid and define shared settings.
mg <-
  model_grid() %>%
  share_settings(
    y = GermanCredit[["Class"]],
    x = GermanCredit %>% select(-Class),
    preProc = "nzv",
    metric = "ROC",
    trControl = trainControl(
      method = "cv",
      number = 5,
      summaryFunction = twoClassSummary,
      classProbs = TRUE
    )
  )

purrr::map_chr(mg$shared_settings, class)

## ------------------------------------------------------------------------
mg <- 
  mg %>%
  add_model(model_name = "Logistic Regression Baseline",
            method = "glm",
            family = binomial(link = "logit"))

mg$models

## ------------------------------------------------------------------------
mg <- 
  mg %>%
  add_model(model_name = "Logistic Regression PCA",
            method = "glm",
            family = binomial(link = "logit"),
            preProc = c("nzv", "center", "scale", "pca")) %>%
  add_model(model_name = "Logistic Regression PCA 98e-2",
            method = "glm",
            family = binomial(link = "logit"),
            preProc = c("nzv", "center", "scale", "pca"),
            custom_control = list(preProcOptions = list(thresh = 0.98)))
            
mg$models

## ------------------------------------------------------------------------
# there are no conflicts.
dplyr::intersect(names(mg$shared_settings), names(mg$models$`Logistic Regression Baseline`))

# consolidate model settings into one model.
consolidate_model(
  mg$shared_settings, 
  mg$models$`Logistic Regression Baseline`
  ) %>%
  purrr::map_chr(class)

## ------------------------------------------------------------------------
# the 'preProc' setting is defined both in the shared and model specific settings.
dplyr::intersect(names(mg$shared_settings), names(mg$models$`Logistic Regression PCA`))

mg$shared_settings$preProc
mg$models$`Logistic Regression PCA`$preProc

# consolidate model settings into one model.
consolidate_model(
  mg$shared_settings, 
  mg$models$`Logistic Regression PCA`
  ) %>%
  magrittr::extract2("preProc")

## ------------------------------------------------------------------------
# the 'trControl$preProcOptions$thresh' setting is defined in the shared
# settings but customized in the model specific settings.

mg$shared_settings$trControl$preProcOptions$thresh
mg$models$`Logistic Regression PCA 98e-2`$custom_control$preProcOptions$thresh

# consolidate model settings into one model.
consolidate_model(
  mg$shared_settings, 
  mg$models$`Logistic Regression PCA 98e-2`
  ) %>%
  magrittr::extract2(c("trControl", "preProcOptions", "thresh"))

## ---- message = FALSE, warning = FALSE-----------------------------------
# train models from model grid.
mg <- train(mg)

# the fitted models now appear in the 'model_fits' component.
names(mg$model_fits)

# extract performance.
mg$model_fits %>%
  caret::resamples(.) %>%
  summary(.)

## ---- warning = FALSE----------------------------------------------------
# train models from model grid.
mg <- 
  mg %>%
  add_model(model_name = "Funky Forest",
            method = "rf") %>%
  train(.)

names(mg$model_fits)

## ---- message = FALSE----------------------------------------------------
# create base recipe.
library(recipes)
rec <- 
  recipe(GermanCredit, formula = Class ~ .) %>%
  step_nzv(all_predictors())

## ---- warning = FALSE----------------------------------------------------
mg_rec <-
  model_grid() %>%
  share_settings(
    metric = "ROC",
    data = GermanCredit,
    trControl = trainControl(
      method = "cv",
      number = 5,
      summaryFunction = twoClassSummary,
      classProbs = TRUE
    )
  ) %>%
  add_model(
    model_name = "Log Reg",
    x = rec,
    method = "glm",
    family = binomial(link = "logit")
  ) %>%
  add_model(
    model_name = "Log Reg PCA",
    x = rec %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors()) %>%
      step_pca(all_predictors()),
    method = "glm",
    family = binomial(link = "logit")
  ) %>%
  train(.)

mg_rec$model_fits %>%
  caret::resamples(.) %>%
  summary(.)

## ------------------------------------------------------------------------
# existing model configuration.
mg$models$`Logistic Regression PCA`

# edit model configuration.
mg <-
  mg %>%
  edit_model(model_name = "Logistic Regression PCA",
             preProc = c("nzv", "center", "scale", "ICA"))

mg$models$`Logistic Regression PCA`

## ------------------------------------------------------------------------
names(mg$models)

# remove model configuration.
mg <-
  mg %>%
  remove_model("Funky Forest")

names(mg$models)

