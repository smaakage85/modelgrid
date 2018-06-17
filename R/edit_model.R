#' Edit model within a model grid
#'
#' Modify an existing model in a model grid.
#'
#' @param model_grid \code{model_grid}
#' @param model_name \code{character} with the unique name (set by the user) of
#' the model, you want to modify.
#' @param ... All the settings you want to modify for an existing model
#' specification.
#'
#' @return \code{model_grid}
#' @export
edit_model <- function(model_grid, model_name, ...) {

  # check if model name exists in model grid.
  if (!(model_name %in%  names(model_grid$models))) stop("model_name is not part of existing model_grid")

  # list new model settings, including updated settings.
  new_settings  <- list(...)

  # keep unchanged existing settings from model.
  keep_settings <- dplyr::setdiff(names(model_grid$models[[model_name]]), names(new_settings))

  # append new and/or changes setting to model.
  updated_model <- append(model_grid$models[[model_name]][keep_settings], new_settings)

  # replace/overwrite existing model with updated model.
  model_grid$models[[model_name]] <- updated_model

  # delete model fits of existing model from model grid.
  if (model_name %in% names(model_grid$model_fits)) {
    model_grid$model_fits <- subset(model_grid$model_fits, names(model_grid$model_fits) != model_name)
    message("Model fit for ", model_name, " has been swiped.")
  }

  # return model grid.
  model_grid

}
