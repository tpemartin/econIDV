#' Create material design icons generator.
#'
#' @return a list of icon functions
#' @export
#'
Icons <- function() {
  iconfile = system.file("material-design-icons/iconnames.json", package="econIDV")
  jsonlite::fromJSON(iconfile) -> iconnames
  iconnames |> purrr::map(generate_iconFun) |>
    setNames(iconnames)
}
generate_iconFun <- function(iconname){
  function(){
    require(htmltools)
    tagList(
      materialDesignIcons(),
      tags$span(class="material-icons", iconname)
    )
  }
}
materialDesignIcons <- function(){
  htmltools::htmlDependency(
    name="material-icon",
    version="1.0.0",
    src=c(href=""),
    stylesheet = "",
    head = htmltools::HTML('<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">')
  )
}
