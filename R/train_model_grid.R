#' Train models within a model grid
#'
#' The S3 method of the train function for the 'model_grid' class consolidates
#' all model (and training) configurations from a model grid and
#' trains them with the train function from the caret package.
#'
#' @param mg \code{model_grid}
#' @param train_all \code{logical} if set to TRUE, all models will be trained.
#' If set to FALSE, only models, for which no fit already exists, will be
#' trained.
#' @param resample_seed \code{integer} is used to create identical resamples
#' across models in order to obtain a fair (and reproducible) comparison of
#' the models. If set to NULL, seed will not be set (NOT advised).
#'
#' @method train model_grid
#' @export
#'
#' @examples
#' \dontrun{
#' # Load dataset and packages.
#' library(caret)
#' library(magrittr)
#' data(GermanCredit)
#'
#' # Create model grid with RF and XGB models.
#' mg <-
#'   model_grid() %>%
#'   share_settings(
#'     y = GermanCredit[["Class"]],
#'     x = GermanCredit %>% select(-Class),
#'     metric = "ROC",
#'     preProc = c("nzv", "center", "scale", "pca"),
#'     trControl = trainControl(
#'       method = "cv",
#'       number = 5,
#'       summaryFunction = twoClassSummary,
#'       classProbs = TRUE
#'     )
#'   ) %>%
#'   add_model(
#'     model_name = "Funky Forest",
#'     method = "rf",
#'     tuneLength = 5,
#'     preProc = c("center", "scale")
#'     ) %>%
#'   add_model(
#'     model_name = "Big Boost",
#'     method = "xgbTree",
#'     nthread = 8
#'     ) %>%
#'   add_model(
#'     model_name = "Big Boost PCA_20",
#'     method = "xgbTree",
#'     custom_control = list(preProcOptions = list(pcaComp = 20))
#'   )
#'
#' # Train all model configurations in model grid.
#' train(mg)
#' }
#' @rdname model_grid
train.model_grid <- function(mg, train_all = FALSE, resample_seed = 123) {

  # check inputs.
  if (length(mg$models) == 0) {
    stop("No models to train.")
  }

  # identify models without corresponding fits.
  models_without_fit <- dplyr::setdiff(names(mg$models), names(mg$model_fits))

  # stop, if all models already have been equipped with a fit.
  if (length(models_without_fit) == 0 & !train_all) {
    stop("It seems all models have already been trained. If you want to ",
         "train all of the models regardless, set train_all to TRUE.")
  }

  # decide what models to train.
  if (length(models_without_fit) != 0 & !train_all) {
    fit_models <-mg$models[models_without_fit]
  } else {
    fit_models <- mg$models
  }

  # consolidate and train models from model grid.
  models_trained <-
    fit_models %>%
    purrr::map2(.x = ., .y = names(.), purrr::safely(.f = function(.x, .y) {
      complete_model <- consolidate_model(mg$shared_settings, .x)
      message(paste0("[", Sys.time(),"] Training of '", .y, "' started."))
      # set seed before training to ensure the same resamples are used for all models.
      if (!is.null(resample_seed)) {set.seed(resample_seed)}
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

  # throw warning, if that is the case.
  if (any(!is_ok)) warning("One or more models threw errors :(! For this reason the fitted models are ",
                           "saved with both a 'result' and an 'error' component. \n \nAre you sure, all ",
                           "inputs are valid? \n\nThe following models were not trained succesfully: \n\n",
                           paste0(names(is_ok[!is_ok]), sep = "\n"))

  # if all models are ok, only return 'result' component.
  if (all(is_ok)) {models_trained <- purrr::map(models_trained, "result")}

  # add trained models to model grid.
  if(identical(fit_models, mg$models)) {
    # in case, that all models have been trained, insert all models.
    mg$model_fits <- models_trained
  } else {
    # if only a subset of the models have been trained, append only these model fits.
    mg$model_fits <- append(mg$model_fits, models_trained)
  }

  # sort trained models in lexicographical order by their names.
  mg$model_fits <- mg$model_fits[sort(names(mg$model_fits))]

  # return model grid.
  mg

}
