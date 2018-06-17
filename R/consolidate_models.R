#' Consolidate model from model grid
#'
#' Consolidate models from shared settings and model specific settings. In case
#' of any overlap between the two, the model specific settings will apply.
#'
#' @param shared_settings \code{list} settings that are shared across all models.
#' @param models \code{list} with the individual specifications for models in
#' a model grid.
#'
#' @return \code{list} with a complete model specification, that can be trained
#' using the 'caret' package.
#'
#' @export
consolidate_models <- function(shared_settings, models) {

  # consolidate model specifications.
  models %>%
    purrr::map(function(x) {
      # modify 'trControl' parameter, if 'custom_control' parameter has been set.
      if ("custom_control" %in% names(x)) {
        shared_settings$trControl <- append(x$custom_control,
                                            shared_settings$trControl[dplyr::setdiff(names(shared_settings$trControl),
                                                                                     names(x$custom_control))])
        # remove 'custom_control' parameter from final model specification.
        x <-
          x[dplyr::setdiff(names(x), "custom_control")]

      }
      append(x, shared_settings[dplyr::setdiff(names(shared_settings), names(x))])
    })

}
