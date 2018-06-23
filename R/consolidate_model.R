#' Consolidate model settings to a complete caret model specification
#'
#' Consolidate model (and model training) settings from shared and model specific
#' settings to a complete caret model specification. In case there is an overlap
#' between the two, the model specific settings will apply.
#'
#' @param shared_settings \code{list}, settings that are shared by all models by
#' default.
#' @param model \code{list}, the individual specifications of a model in a
#' model grid.
#'
#' @return \code{list}, a complete model and training specification, that
#' can be trained with caret.
#'
#' @export
#'
#' @examples
#' # create model grid.
#' mg <-
#'   model_grid() %>%
#'   share_settings(y = iris[["Species"]],
#'                  x = iris %>% dplyr::select(-Species),
#'                  trControl = caret::trainControl()) %>%
#'   add_model("FunkyForest", method = "rf",
#'             preProc = c("center", "scale", "pca"),
#'             custom_control = list(preProcOptions = list(thresh = 0.8)))
#'
#' # consolidate all settings to complete caret model specification.
#' consolidate_model(mg$shared_settings, mg$models$FunkyForest)
consolidate_model <- function(shared_settings, model) {

  # check inputs.
  if (exists("custom_control", model) && !exists("trControl", shared_settings)) {
    stop("'custom_control' argument has been provided, but no 'trControl' ",
         "component has been specified in 'shared_settings'.")
  }

  # modify 'trControl' argument, if 'custom_control' parameter has been set.
  if (exists("custom_control", model) && exists("trControl", shared_settings)) {

    shared_settings$trControl <-
      append(model$custom_control, shared_settings$trControl[
        dplyr::setdiff(names(shared_settings$trControl), names(model$custom_control))])

        # remove 'custom_control' parameter from final model specification.
        model <- model[dplyr::setdiff(names(model), "custom_control")]

      }

  # append only relevant shared settings for a given model.
  append(model, shared_settings[dplyr::setdiff(names(shared_settings), names(model))])

}
