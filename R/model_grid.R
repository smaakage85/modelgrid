#' Pre-allocate an empty model grid
#'
#' @return \code{model_grid}
#' @export
#'
#' @examples
#' # Pre-allocate an empty model grid.
#' model_grid()
model_grid <- function() {

  # create list with the structure of a 'model_grid'.
  model_grid <- list(shared_settings = NULL, models = NULL, model_fits = NULL)

  # set class of object to "model_grid".
  class(model_grid) <- "model_grid"

  # return model_grid.
  model_grid

}
