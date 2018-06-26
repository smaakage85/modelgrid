#' Edits model within a model grid
#'
#' Modifies an existing model (and training) specification in a model grid.
#'
#' @param model_grid \code{model_grid}
#' @param model_name \code{character}, the unique name (as set by the user) of
#' the model, that should be modified.
#' @param ... All the model (and model training) settings you want to modify for
#'  an existing model specification.
#'
#' @return \code{model_grid}
#'
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' # Creates model grid and add random forest model.
#' mg <-
#'   model_grid() %>%
#'   add_model(model_name = "Random Forest Test", method = "rf", tuneLength = 5)
#'
#' # Edits the size of tuning grid of the random forest model.
#' edit_model(mg, model_name = "Random Forest Test", tuneLength = 10)
edit_model <- function(model_grid, model_name, ...) {

  # checks if the model_grid is in fact a model_grid.
  if (!inherits(model_grid, "model_grid")) {
    stop("The 'model_grid' must inherit from the 'model_grid' class.")
  }

  # checks if a model with that name exists in model grid.
  if (!exists(model_name, model_grid[["models"]])) {
    stop("There is no model with that name within the model grid.")
  }

  # checks 'method' (if provided).
  if (exists("method", list(...)) && !(list(...)[["method"]] %in% caret::modelLookup()$model)) {
    stop("'method' is not supported by this version of caret.")
  }

  # creates list with settings to be updated.
  new_settings  <- list(...)

  # keeps unchanged settings from existing model.
  keep_settings <- dplyr::setdiff(names(model_grid$models[[model_name]]), names(new_settings))

  # appends new settings and/or changes to model.
  updated_model <- append(model_grid$models[[model_name]][keep_settings], new_settings)

  # deletes model from model grid.
  if (exists(model_name, model_grid[["model_fits"]])) {
    model_grid <- remove_model(model_grid, model_name)
  }
  
  # replaces with updated model.
  model_grid$models[[model_name]] <- updated_model
  
  # returns model grid.
  model_grid

}
