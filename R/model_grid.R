#' Pre-allocate an empty model grid
#'
#' Pre-allocate an empty model grid. The model grid enables the user to
#' construct, manage and train multiple caret models. Bla bla bla.
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
  model_grid <- list(shared_settings = NULL, models = NULL, model_fits = NULL)

  # set class of object to "model_grid".
  class(model_grid) <- "model_grid"

  # return model_grid.
  model_grid

}
