#' Pre-allocate an empty model grid
#'
#' This is a constructor function that pre-allocates an empty model grid.
#'
#' The model grid makes it easy to create, manage and train multiple caret
#' models.
#'
#' @return \code{model_grid}
#'
#' @export
#'
#' @seealso \code{\link{add_model}} for how to add a model to a model grid,
#'  \code{\link{edit_model}} for how to edit an existing model within a model grid,
#'  \code{\link{share_settings}} for how to define the shared settings of models
#'  within a model grid and
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
