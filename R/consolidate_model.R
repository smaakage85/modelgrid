#' Consolidate model from model grid
#'
#' Consolidate model settings from shared settings and model specific settings.
#' In case of any overlap between the two, the model specific settings will
#' apply.
#'
#' @param shared_settings \code{list} settings that are shared across all models.
#' @param model \code{list} with the individual specifications of a model in a
#' model grid.
#'
#' @return \code{list} with a complete model specification, that can be trained
#' using the 'caret' package.
#'
#' @export
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
