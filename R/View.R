#' View tools to view x, function inputs
#'
#' @return
#' @export
#'
#' @examples none
ViewDetail <- function(){
  view <- list()
  view$x <- showX
  view$inputs <- function(fn){
    fn |>
      get_inputNamesDefaults() -> inputArguments
      utils::View(inputArguments)
  }
  return(view)
}
