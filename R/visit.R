#' Visit plotly leaflet reference
#'
#' @return
#' @export
#'
#' @examples none
Visit <- function(){
  visit <- list()
  visit$plotlyJS <- visit_plotlyjs
  visit$plotlyR <- visit_plotlyr
  visit$leafletJS <- visit_leafletjs
  visit$leafletR <- visit_leafletr
  visit$datatableJS <- visit_datatablejs
  visit$datatableR <- visit_datatabler
  return(visit)
}

visit_datatabler <-function(){
  browseURL(
    "https://rstudio.github.io/DT/"
  )
}
visit_datatablejs <- function(){
  browseURL(
    "https://datatables.net/"
  )
}
visit_plotlyjs <- function(){
  browseURL(
    "https://plotly.com/javascript/reference/"
  )
}
visit_plotlyr <- function(){
  browseURL(
    "https://plotly.com/r/"
  )
}
visit_leafletjs <- function(){
  browseURL(
    "https://leafletjs.com/reference.html"
  )
}
visit_leafletr <- function(){
  browseURL(
    "https://rstudio.github.io/leaflet/"
  )
}
