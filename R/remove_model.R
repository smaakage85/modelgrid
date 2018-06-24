#' Remove model from model grid
#'
#' Removes an individual model specification from a model grid. If the model has
#' been trained, the fitted model will also be swiped.
#'
#' @param model_grid \code{model_grid}
#' @param model_name \code{character}, the unique name (set by the user) of
#' the model, which will be removed from a model grid.
#'
#' @return \code{model_grid}
#' @export
#'
#' @examples
#' library(magrittr)
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

  # check inputs.
  if (!inherits(model_grid, "model_grid")) {
    stop("The 'model_grid' must inherit from the 'model_grid' class.")
  }

  if (length(model_grid$models) == 0) {
    stop("no model specifications have been defined within the model grid.")
    }

  # check if a model with that name exists in the model grid.
  if (!(exists(model_name, model_grid[["models"]]))) {
    stop("there is no model with that name in the model_grid.")
  }

  # delete model specification.
  model_grid$models <-
    subset(model_grid$models, names(model_grid$models) != model_name)

  # delete any existing model fit.
  if (exists(model_name, model_grid[["model_fits"]])) {
    model_grid$model_fits <-
      subset(model_grid$model_fits, names(model_grid$model_fits) != model_name)
    message("Model fit for ", model_name, " has been swiped.")
  }

  # return model grid.
  model_grid

}
