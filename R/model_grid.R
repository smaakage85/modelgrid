#' Create a caret model grid
#'
#' Train and compare multiple models with a caret model grid.
#'
#' \describe{
#'   \item{add_model}{Add model to model grid.}
#'   \item{consolidate_models}{Consolidate models from shared settings and individual model
#'   specifications.}
#'   \item{edit_model}{Edit model within a model grid.}
#'   \item{model_grid}{Create empty model grid.}
#'   \item{remove_model}{Remove model from model grid.}
#'   \item{set_shared_settings}{Set shared settings for all models within a model grid.}
#'   \item{xtrain}{Consolidate and train model configurations from a model grid.}
#' }
#'
#' @param custom_control list, any customizations to the shared 'trControl' argument.
#' @param model_grid model_grid.
#' @param model_name character, your name for a given model.
#' @param models list, list with model (specifications) from a model grid.
#' @param resample_seed integer, this seed is used to create identical resamples across models in
#' order to obtain a fair comparison of the models.
#' @param shared_settings list, settings that are shared across all models.
#' @param train_all logical, if TRUE train all models. If set to FALSE train only models,
#' for which no fit already exists.
#' @param ... Optional arguments.
#'
#' @rdname model_grid
#' @export
model_grid <- function() {

  # create list with appropriate structure.
  model_grid <-
    list(shared_settings = NULL,
         models = NULL,
         model_fits = NULL)

  # set S3 class to "model_grid".
  class(model_grid) <-
    "model_grid"

  # return model_grid
  model_grid

}

#' @rdname model_grid
#' @export
set_shared_settings <- function(model_grid, ...) {

  # check inputs.
  if (!inherits(model_grid, "model_grid")) stop("The 'model_grid' argument must inherit from the 'model_grid' class.")
  if (!"trControl" %in% names(list(...))) stop("'trControl' parameter has not been set.")
  if (!is.null(model_grid$model_fits)) {
    model_grid$model_fits <- NULL
    message("All model fits have been swiped due to shared settings being updated.")
  }

  # set shared settings of the model grid.
  model_grid[["shared_settings"]] <-
    list(...)

  # return model grid with updated shared settings.
  model_grid

}

#' @rdname model_grid
#' @export
add_model <- function(model_grid,
                      model_name = NULL,
                      custom_control = NULL,
                      ...) {

  # check inputs.
  if (!inherits(model_grid, "model_grid")) stop("The 'model_grid' argument inherit from the 'model_grid' class.")
  if (!is.null(model_name) && model_name %in% names(model_grid$models))
    stop("Model names should be unique. That name is already taken.")
  if ("trControl" %in% names(list(...))) stop("Do not set the 'trControl' parameter directly. Please make any model specific
                                              customizations to the shared 'trControl' parameter with the
                                              'custom_control' parameter.")
  if ("method" %in% names(list(...)) && !(list(...)[["method"]] %in% caret::modelLookup()$model)) {
    stop("'method' is not supported by this version of caret.")
  }

  # set model name automatically, if it has not already been set.
  if (is.null(model_name)) {
    if (is.null(model_grid$models)) {
      model_name <- "Model1"
    } else {
      model_name <-
        dplyr::setdiff(
          paste0("Model", 1:(length(model_grid$models) + 1)),
          names(model_grid$models)) %>%
        sort(.) %>%
        magrittr::extract2(1)
    }
  }

  # add model to grid.
  model_grid[["models"]][[model_name]] <- list(...)
  # set any customizations to 'trControl'.
  if (!is.null(custom_control)) model_grid[["models"]][[model_name]][["custom_control"]] <- custom_control

  # sort models by name.
  model_grid[["models"]] <-
    model_grid[["models"]][sort(names(model_grid[["models"]]))]

  # return model grid with the addition of the specified model.
  model_grid

}

#' @rdname model_grid
consolidate_models <- function(shared_settings, models) {

  # consolidate model specifications.
  consolidated_models <-
    models %>%
    lapply(., function(x) {
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

  # set 'model_name' attribute for all model specifications.
  for (i in names(models)) {
    attr(consolidated_models[[i]], "model_name") <- i
  }

  # return consolidated model specifications.
  consolidated_models
}

#' @rdname model_grid
#' @export
xtrain <-
  function(model_grid,
           train_all = FALSE,
           resample_seed = 1,
           ...) {

    # check inputs.
    if (is.null(model_grid$models)) stop("No models to train.")

    # identify models without corresponding fits.
    models_without_fit <-
      dplyr::setdiff(
        names(model_grid$models),
        names(model_grid$model_fits)
      )

    # stop, if all models already have been equipped with a fit.
    if (length(models_without_fit) == 0 & !train_all) stop("All models have already been trained. If you want to
                                                           train all of the models regardless, set train_all to TRUE.")
    # decide what models to train.
    if (length(models_without_fit) != 0 & !train_all) {
      fit_models <-
        model_grid$models[models_without_fit]
    } else {
      fit_models <-
        model_grid$models
    }

    # train models.
    models_trained <-
      consolidate_models(model_grid[["shared_settings"]], fit_models) %>%
      lapply(., function(x, ...) {
        message(paste0("[", Sys.time(),"] Training of '", attributes(x)[["model_name"]], "' started."))
        if (x$trControl$allowParallel) {doMC::registerDoMC(...)}
        # set seed before training to ensure the same resamples are used for all models
        set.seed(resample_seed)
        # estimate models
        models <- try(do.call(caret::train, x), silent = TRUE)
        message(paste0("[", Sys.time(),"] Training of '", attributes(x)[["model_name"]], "' completed."))
        # return models.
        models
      }
      )

    # equip models with model name attribute.
    for (i in 1:length(names(fit_models))) {
      attr(models_trained[[i]], "model_name") <- names(fit_models$models)[i]
    }

    # did any calls to caret::train fail?
    models_failed <-
      vapply(
        X = names(models_trained),
        FUN = function(x) {
          inherits(models_trained[[x]], "try-error")
        },
        FUN.VALUE = logical(1)
      )

    # throw warning message, if that is the case.
    if (any(models_failed)) message("Not all models were trained succesfully. Check inputs.")

    # add trained models to model grid.
    if(identical(fit_models, model_grid$models)) {
      # in case, that all models have been trained, insert all models.
      model_grid$model_fits <-
        models_trained
    } else {
      # if only a subset of the models have been trained, append these model fits.
      model_grid$model_fits <-
        append(model_grid$model_fits, models_trained)
    }

    # sort trained models by name.
    model_grid$model_fits <-
      model_grid$model_fits[sort(names(model_grid$model_fits))]

    # return model grid.
    model_grid

  }

#' @rdname model_grid
#' @export
edit_model <-
  function(model_grid, model_name, ...) {

    # check if model name exists in model grid
    if (!(model_name %in%  names(model_grid$models))) stop("model_name is not part of existing model_grid")

    # list new model settings, including updated settings.
    new_settings  <- list(...)

    # keep unchanged existing settings from model.
    keep_settings <- dplyr::setdiff(names(model_grid$models[[model_name]]), names(new_settings))

    # append new and/or changes setting to model.
    updated_model <- append(model_grid$models[[model_name]][keep_settings], new_settings)

    # replace/overwrite existing model with updated model.
    model_grid$models[[model_name]] <- updated_model

    # delete model fits of existing model from model grid
    if (model_name %in% names(model_grid$model_fits)) {
      model_grid$model_fits <- subset(model_grid$model_fits, names(model_grid$model_fits) != model_name)
      message("Model fit for ", model_name, " has been swiped.")
    }

    # return model grid
    return(model_grid)
  }

#' @rdname model_grid
#' @export
remove_model <-
  function(model_grid, model_name) {

    # check if model name exists in model grid
    if (!(model_name %in% names(model_grid$models))) stop("model_name is not in the model_grid.")

    # delete model from models
    model_grid$models <- subset(model_grid$models, names(model_grid$models) != model_name)

    # delete model fits of existing model from model grid
    model_grid$model_fits <- subset(model_grid$model_fits, names(model_grid$model_fits) != model_name)

    # return model grid
    return(model_grid)
  }
