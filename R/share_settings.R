#' Set shared settings of a model grid
#'
#' Set shared settings for all model (and training) configurations within a
#' model grid. These settings will apply for any given model, unless the same
#' settings have already been specified in the model specific configurations. In
#' that case, the model specific settings will apply.
#'
#' @param model_grid \code{model_grid}
#' @param ... All optional shared settings.
#'
#' @return \code{model_grid} equipped with shared settings.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(caret)
#' library(dplyr)
#' data(GermanCredit)
#'
#' # Pre-allocate empty model grid.
#' models <- model_grid()
#'
#' # Set shared settings of model grid.
#' models %>%
#'   share_settings(
#'     y = GermanCredit[["Class"]],
#'     x = GermanCredit %>% select(-Class),
#'     metric = "ROC",
#'     preProc = c("center", "scale", "pca"),
#'     trControl = trainControl(
#'       method = "cv",
#'       number = 5,
#'       summaryFunction = twoClassSummary,
#'       classProbs = TRUE
#'       )
#'   )
share_settings <- function(model_grid, ...) {

  # check inputs.
  if (!inherits(model_grid, "model_grid")) {
    stop("The 'model_grid' must inherit from the 'model_grid' class.")
  }

  if (length(list(...)) == 0) {
    stop("'trControl' parameter has not been set. It is often useful to ",
         "set the 'trControl' argument as a shared setting.")
  }

  if (!exists("trControl", list(...))) {
    warning("'trControl' parameter has not been set. It is often useful to ",
            "set the 'trControl' argument as a shared setting.")
  }

  if (length(model_grid$model_fits) != 0) {
    model_grid$model_fits <- list()
    message("All model fits have been swiped due to shared settings being updated.")
  }
  # check validity of method (if provided).
  if ("method" %in% names(list(...)) && !(list(...)[["method"]] %in% caret::modelLookup()$model)) {
    stop("'method' is not supported by this version of caret.")
  }

  # set shared settings of the model grid.
  model_grid[["shared_settings"]] <- list(...)

  # return model grid with updated shared settings.
  model_grid

}
