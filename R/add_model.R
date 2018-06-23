#' Add a model specification to a model grid
#'
#' Define and add an individual model (and training) specification to an
#' existing model grid.
#'
#' @param model_grid \code{model_grid}
#' @param model_name \code{character}, custom name for a given model. Must be
#' unique within the model grid.
#' @param custom_control \code{list}, any customizations to subsettings of the 'trControl'
#' component from the 'shared_settings' of the model grid (requires that 'trControl' has
#' actually been provided as part of the shared settings).
#' @param ... All (optional) individual settings (including training settings)
#' that the user wishes to apply to the specific model.
#'
#' @return \code{model_grid} with an additional individual model
#' specification.
#'
#' @export
#'
#' @examples
#' # Pre-allocate empty model grid.
#' mg <- model_grid()
#'
#' # Add 'random forest' model spec.
#' mg <-
#'   mg %>%
#'   add_model(model_name = "Random Forest Test", method = "rf", tuneLength = 5)
add_model <- function(model_grid, model_name = NULL, custom_control = NULL, ...) {

  # check inputs.
  if (is.null(custom_control) && length(list(...)) == 0) {
    stop("No model specific settings were specified.")
    }

  if (!inherits(model_grid, "model_grid")) {
    stop("The 'model_grid' argument must inherit from the 'model_grid' class.")
    }

  if (!is.null(model_name) && exists(model_name, model_grid["models"])) {
    stop("Model names should be unique. That name is already taken.")
  }

  if (!is.null(custom_control) && exists("trControl", list(...))) {
    stop("It is not meaningful to provide BOTH 'custom_control' and 'trControl' arguments in the
          model specific configuration.")
    }

  if (!is.null(custom_control) && exists("trControl", model_grid["shared_settings"])) {
    stop("'custom_control' argument has been provided, but no 'trControl' component has been
          specified within 'shared_settings'.")
  }

  if (exists("method", list(...)) && !(list(...)[["method"]] %in% caret::modelLookup()[["model"]])) {
    stop("'method' is not supported by this version of caret.")
  }

  # set model name automatically, if it has not already been set.
  if (is.null(model_name)) {
    if (is.null(model_grid$models)) {
      # start indexing from zero.
      model_name <- "Model0"
    } else {
      model_name <-
        dplyr::setdiff(
          paste0("Model", c(0, seq_along(model_grid$models))),
          names(model_grid$models)
        ) %>%
        sort(.) %>%
        magrittr::extract2(1)
    }
  }

  # add model to grid.
  model_grid[["models"]][[model_name]] <- list(...)

  # make any customizations to 'trControl'.
  if (!is.null(custom_control)) {
    model_grid[["models"]][[model_name]][["custom_control"]] <- custom_control
  }

  # sort models by name.
  model_grid[["models"]] <-
    model_grid[["models"]][sort(names(model_grid[["models"]]))]

  # return model grid with the addition of the specified model.
  model_grid

}
