#' Visit plotly leaflet reference
#'
#' @return
#' @export
#'
#' @examples none
Visit <- function(){
  visit <- list()
  visit$plotlyjs <- visit_plotlyjs
  visit$leafletjs <- visit_leafletjs
  return(visit)
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
find_li_with_attribute <- function(all_li, attr){
  purrr::map(
    all_li,
    ~{
      .x |>
        rvest::html_elements("a.attribute-name") |>
        rvest::html_text() }) -> list_attributenames
  purrr::map_lgl(
    list_attributenames,
    ~{
      any(stringr::str_detect(.x, glue::glue("\\b{attr}\\b")))
    }
  ) |> which() -> whichHasTargetAttribute
  all_li[whichHasTargetAttribute]
}
