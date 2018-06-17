#' Train models within a model grid
#'
#' Consolidates all model configurations from a model grid and trains them with
#' caret::train.
#'
#' @param x \code{model_grid}
#' @param train_all \code{logical} if TRUE train all models. If set to FALSE train only models,
#' for which no fit already exists.
#' @param resample_seed \code{integer} is used to create identical resamples across models in
#' order to obtain a fair (and reproducible) comparison of the models.
#'
#' @method train model_grid
#' @export
train.model_grid <- function(x, train_all = FALSE, resample_seed = 0) {

  # check inputs.
  if (is.null(x$models)) stop("No models to train.")

  # identify models without corresponding fits.
  models_without_fit <- dplyr::setdiff(names(x$models), names(x$model_fits))

  # stop, if all models already have been equipped with a fit.
  if (length(models_without_fit) == 0 & !train_all) stop("It seems all models have already been trained. If you want to
                                                         train all of the models regardless, set train_all to TRUE.")
  # decide what models to train.
  if (length(models_without_fit) != 0 & !train_all) {
    fit_models <-x$models[models_without_fit]
  } else {
    fit_models <- x$models
  }

  # consolidate and train models.
  models_trained <-
    fit_models %>%
    purrr::map2(.x = ., .y = names(.), purrr::safely(.f = function(.x, .y) {
      complete_model <- consolidate_models(x$shared_settings, .x)
      message(paste0("[", Sys.time(),"] Training of '", .y, "' started."))
      # set seed before training to ensure the same resamples are used for all models.
      set.seed(resample_seed)
      # train model.
      model <- do.call(caret::train, complete_model)
      message(paste0("[", Sys.time(),"] Training of '", .y, "' completed."))
      # return trained model.
      model
    }))


  # did any calls to caret::train fail?
  is_ok <-
    models_trained %>%
    purrr::transpose(.) %>%
    magrittr::extract2("error") %>%
    purrr::map_lgl(is.null)

  # throw message, if that is the case.
  if (any(!is_ok)) cat("One or more models threw errors :(! Are you sure, all inputs are valid? \n Errors: \n",
                       paste0(names(is_ok[!is_ok]), sep = "\n"))

  # if all models are ok, only return 'result' component.
  if (all(is_ok)) {models_trained <- purrr::map(models_trained, "result")}

  # add trained models to model grid.
  if(identical(fit_models, x$models)) {
    # in case, that all models have been trained, insert all models.
    x$model_fits <- models_trained
  } else {
    # if only a subset of the models have been trained, append only these model fits.
    x$model_fits <- append(x$model_fits, models_trained)
  }

  # sort trained models in lexicographical order by their names.
  x$model_fits <- x$model_fits[sort(names(x$model_fits))]

  # return model grid.
  x

}
