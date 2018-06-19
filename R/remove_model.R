#' Remove model from model grid
#'
#' Removes an individual model specification from a model grid. If the model has
#' been trained, the fitted model will also be swiped.
#'
#' @param model_grid \code{model_grid}
#' @param model_name \code{character} with the unique name (set by the user) of
#' the model, which will be removed from a model grid.
#'
#' @return \code{model_grid}
#' @export
#'
#' @examples
#' # Pre-allocate empty model grid.
#' mg <- model_grid()
#'
#' # Add random forest model.
#' mg <-
#'   mg %>%
#'   add_model(model_name = "Random Forest Test", method = "rf", tuneLength = 5)
#'
#' # Remove random forest model again.
#' remove_model(mg, model_name = "Random Forest Test")
remove_model <- function(model_grid, model_name) {

  # check if model name exists in model grid.
  if (!(model_name %in% names(model_grid$models))) stop("model_name is not in the model_grid.")

  # delete model from models.
  model_grid$models <- subset(model_grid$models, names(model_grid$models) != model_name)

  # delete model fits of existing model from model grid.
  model_grid$model_fits <- subset(model_grid$model_fits, names(model_grid$model_fits) != model_name)

  # return model grid.
  model_grid

}
