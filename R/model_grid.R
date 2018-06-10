#' Create and train a model grid
#'
#' Experiment with and train multiple models in a structured way with a model grid.
#'
#' \describe{
#'   \item{add_model}{Add model to model grid.}
#'   \item{consolidate_models}{Consolidate models from shared settings and individual model
#'   specifications.}
#'   \item{edit_model}{Edit model within a model grid.}
#'   \item{model_grid}{Create (empty) model grid.}
#'   \item{remove_model}{Remove model from model grid.}
#'   \item{share_settings}{Set shared settings for all models within a model grid.}
#' }
#'
#' @param custom_control \code{list}, any customizations to the shared 'trControl' argument.
#' @param model_grid \code{model_grid} with correct structure.
#' @param model_name \code{character} with your name for a given model. Must be uniqued within a model grid.
#' @param models \code{list} with the individual specifications for models in a model grid.
#' @param resample_seed \code{integer} is used to create identical resamples across models in
#' order to obtain a fair (and reproducible) comparison of the models.
#' @param shared_settings \code{list} settings that are shared across all models.
#' @param train_all \code{logical} if TRUE train all models. If set to FALSE train only models,
#' for which no fit already exists.
#' @param x \code{model grid} with correct structure.
#' @param ... Optional arguments.
#'
#' @rdname model_grid
#' @export
model_grid <- function() {

  # create list with the structure of a 'model_grid'.
  model_grid <- list(shared_settings = NULL, models = NULL, model_fits = NULL)

  # set class of object to "model_grid".
  class(model_grid) <- "model_grid"

  # return model_grid
  model_grid

}

#' @rdname model_grid
#' @export
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

#' @rdname model_grid
#' @export
add_model <- function(model_grid, model_name = NULL, custom_control = NULL, ...) {

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
      # start indexing from zero.
      model_name <- "Model0"
    } else {
      model_name <-
        dplyr::setdiff(
          paste0("Model", c(0, seq_along(mg$models))),
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

#' @rdname model_grid
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

#' @rdname model_grid
#' @method train model_grid
#' @export
train.model_grid <- function(x, train_all = FALSE, resample_seed = 0) {

  # check inputs.
  if (is.null(x$models)) stop("No models to train.")

  # identify models without corresponding fits.
  models_without_fit <- dplyr::setdiff(names(x$models), names(x$model_fits)
    )

  # stop, if all models already have been equipped with a fit.
  if (length(models_without_fit) == 0 & !train_all) stop("All models have already been trained. If you want to
                                                         train all of the models regardless, set train_all to TRUE.")
  # decide what models to train.
  if (length(models_without_fit) != 0 & !train_all) {
    fit_models <-x$models[models_without_fit]
  } else {
    fit_models <- x$models
  }

  # consolidate and train models.
  models_trained <-
    consolidate_models(x[["shared_settings"]], fit_models) %>%
    purrr::map2(.x = ., .y = names(.), purrr::safely(.f = function(.x, .y) {
      message(paste0("[", Sys.time(),"] Training of '", .y, "' started."))
      # set seed before training to ensure the same resamples are used for all models.
      set.seed(resample_seed)
      # train models.
      models <- do.call(caret::train, .x)
      message(paste0("[", Sys.time(),"] Training of '", .y, "' completed."))
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

  # if all models are ok, only return result component
  if (all(is_ok)) {models_trained <- purrr::map(models_trained, "result")}

  # add trained models to model grid.
  if(identical(fit_models, x$models)) {
    # in case, that all models have been trained, insert all models.
    x$model_fits <- models_trained
  } else {
    # if only a subset of the models have been trained, append only these model fits.
    x$model_fits <- append(x$model_fits, models_trained)
  }

  # sort trained models in lexicographical order by name.
  x$model_fits <- x$model_fits[sort(names(x$model_fits))]

  # return model grid.
  x

}
