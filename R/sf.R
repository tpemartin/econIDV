#' Remove sf data frame with empty geometry
#' @description When a sf data frame has rows of empty geometry, ggplotly would produce error.
#'
#' @param sf_elections a sf data frame
#'
#' @return
#' @export
#'
#' @examples none.
remove_emptyGeometry <- function(sf_elections) {
  which(sf::st_is_empty(sf_elections$geometry)) -> whichGeomEmpty
  sf_elections[-whichGeomEmpty,] -> sf_elections
  sf_elections
}
