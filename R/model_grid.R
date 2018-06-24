#' Pre-allocate an empty model grid
#'
#' Constructor function that pre-allocates an empty model grid.
#' The model grid  makes it easy to create, manage and train multiple caret models.
#' Define the settings that by default are to be shared by all of the models in
#' the model grid with \code{\link{share_settings}}. Add the individual
#' specifications for the models you want to investigate with \code{\link{add_model}}.
#' Train all of the models in the model grid with \code{\link{train}}.
#'
#' @return \code{model_grid}
#'
#' @export
#'
#' @seealso \code{\link{add_model}} for how to add a model to a model grid,
#'  \code{\link{edit_model}} for how to edit an existing model within a model grid,
#'  \code{\link{share_settings}} for how to define the shared settings of models
#'  within a model grid, \code{\link{consolidate_model}} for how to consolidate
#'  the shared settings of a model grid and the individual settings of a given
#'  model into one complete caret model configuration and
#'  \code{\link{remove_model}} for how to remove a model from a model grid.
#'
#' @examples
#' # Pre-allocate an empty model grid.
#' model_grid()
#'
model_grid <- function() {

  # create list with the structure of a 'model_grid'.
  model_grid <- list(shared_settings = list(), models = list(), model_fits = list())

  # set class of object to "model_grid".
  class(model_grid) <- "model_grid"

  # return model_grid.
  model_grid

}
