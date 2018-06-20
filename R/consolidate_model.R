#' Consolidate model and training settings in model grid
#'
#' Consolidate model and training settings from the shared settings and the model
#' specific settings. In case there is an overlap between the two, the model
#' specific settings will apply.
#'
#' @param shared_settings \code{list} settings that are shared by all models.
#' @param model \code{list} with the individual specifications of a model in a
#' model grid.
#'
#' @return \code{list} with a complete model and training specification, that
#' can be trained using the 'caret' package.
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' mg <-
#'   model_grid() %>%
#'   share_settings(y = iris[["Species"]],
#'                  x = iris %>% dplyr::select(-Species),
#'                  trControl = caret::trainControl()) %>%
#'   add_model("FunkyForest", method = "rf",
#'             preProc = c("center", "scale", "pca"),
#'             custom_control = list(preProcOptions = list(thresh = 0.8)))
#'
#' consolidate_model(mg$shared_settings, mg$models$FunkyForest)
consolidate_model <- function(shared_settings, model) {

  # modify 'trControl' parameter, if 'custom_control' parameter has been set.
  if ("custom_control" %in% names(model)) {
    shared_settings$trControl <-
      append(model$custom_control, shared_settings$trControl[dplyr::setdiff(names(shared_settings$trControl),
                                                                            names(model$custom_control))])
        # remove 'custom_control' parameter from final model specification.
        model <- model[dplyr::setdiff(names(model), "custom_control")]
      }

  # append only relevant shared settings for a given model.
  append(model, shared_settings[dplyr::setdiff(names(shared_settings), names(model))])

}
