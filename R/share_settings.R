#' Set shared settings for a model grid
#'
#' Set shared settings for all models within a model grid. These settings will
#' apply for a given model, unless the same settings have already been specified
#' in the model specific configuration. In that case the model specific settings
#' will apply. The 'trControl' parameter must be included in the shared
#' settings.
#'
#' @param model_grid \code{model_grid}
#' @param ... Optional arguments.
#'
#' @return \code{model_grid} equipped with shared settings.
#' @export
#'
#' @examples
#' # Load data set.
#' library(caret)
#' data(GermanCredit)
#'
#' # Pre-allocate empty model grid.
#' models <- model_grid()
#'
#' # Set shared settings of model grid.
#' models %>%
#'   share_settings(
#'     y = GermanCredit[["Class"]],
#'     x = GermanCredit %>% dplyr::select(-Class),
#'     metric = "ROC",
#'     trControl = caret::trainControl(
#'       method = "cv",
#'       number = 5,
#'       summaryFunction = caret::twoClassSummary,
#'       classProbs = TRUE
#'       )
#'   )
share_settings <- function(model_grid, ...) {

  # check inputs.
  if (!inherits(model_grid, "model_grid")) stop("The 'model_grid' must inherit from the 'model_grid' class.")
  if (!"trControl" %in% names(list(...))) stop("'trControl' parameter has not been set (as required).")
  if (!is.null(model_grid$model_fits)) {
    model_grid$model_fits <- NULL
    message("All model fits have been swiped due to shared settings being updated.")
  }

  # apply shared settings of the model grid.
  model_grid[["shared_settings"]] <- list(...)

  # return model grid with updated shared settings.
  model_grid

}
