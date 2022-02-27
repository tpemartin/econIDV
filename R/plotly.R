#' Visit plotly leaflet reference
#'
#' @return
#' @export
#'
#' @examples none
Visit <- function(){
  visit <- list()
  visit$plotly <- visit_plotly
  visit$leaflet <- visit_leaflet
  return(visit)
}
visit_plotly <- function(){
  browseURL(
    "https://plotly.com/javascript/reference/"
  )
}
visit_leaflet <- function(){
  browseURL(
    "https://leafletjs.com/reference.html"
  )
}
