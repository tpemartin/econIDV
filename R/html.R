#' Generate diverging two party discrete color guide
#'
#' @param parties a list of two characters
#' @param colorcodes a list of two character vectors with equal length. Each element value represents the discrete colors of a party
#' @param numbers the cut numbers. If each party has 5 colors, then there will be 6 cut numbers.
#'
#' @return a html tag
#' @export
#'
#' @examples
guide_color2 = function(parties, colorcodes, numbers){
  .colorBar1 <- colorBar(numbers, colorcodes[[1]])
  .colorBar2 <- colorBar2(colorcodes[[2]])
  div(
    guide(parties[[1]], .colorBar1,
      .css=css("margin-top"="15px")),
    guide(parties[[2]], .colorBar2)
  )
}
guideColor_dependency = function(){
  htmlDependency(
    name="guideColor",
    version="1.0.0",
    src=c(file=system.file("html/css", package="econIDV")),
    style="guide_color.css"
  )
}
colorBar2 = function(colorcodes){
  purrr::map(colorcodes,
    ~{
      tagList(
        colorItem(.x))}) ->
    list_colorItems
  do.call(colorbarMethod2, list_colorItems) |>
    tagList(
      guideColor_dependency()
    )
}
colorBar = function(numbers, colorcodes){
  nNumber=length(numbers)
  lastNumber = numbers[[nNumber]]
  numbers=numbers[1:(nNumber-1)]
  purrr::map2(numbers,colorcodes,
    ~{
      tagList(
        tickItem(.x),
        colorItem(.y))}) -> list_colors
  append(list_colors,
    list(
      tickItem(lastNumber)
    ))->
    list_colorItems
  do.call(colorbarMethod, list_colorItems) |>
    tagList(
      guideColor_dependency()
    )
}
tickItem = function(number){
  div(class="tick",
    div(class="tickNumber", number))
}
colorItem = function(colorcode){
  .css=css("background-color"=colorcode)
  div(class="colorItem", style=.css)
}

colorbarMethod = function(...){
  require(htmltools)
  div(class="colorbar", ...)
}
colorbarMethod2 = function(...){
  require(htmltools)
  div(class="colorbar2", ...)
}
guide = function(party, .colorBar, .css=NULL){
  div(class="guide",
    div(class="guideText",
      style=.css, party),
    .colorBar)
}



fig_import <- function(econWeb, Fig) {
  fig = econWeb::Fig()
  fig$export("inst/html/css/geoPopup")
}
map_query <- function(location){
  glue::glue("https://www.google.com/maps/search/?api=1&query={location}")
}
tag_geoPopup <-function(date, time, location){
  tags$div(class = "geoCard",
    tags$div(class = "geoCard-time",
      tags$div(class="geoCard-time-date", date),
      tags$div(class="geoCard-time-time",time)),
    tags$div(class = "geoCard-buttonMap",
      tags$a(
        href=map_query(location),
        target="_blank",
        tag_mappin()
      )
      ))
}
geoPopup_dependency <- function(){
  htmltools::htmlDependency(
    name="econIDV",
    version="1.0.0",
    src=c(file=system.file("/html",  package = "econIDV")),
    style="/css/geoPopup.css",
    all_files = T
  )}
ui_geoPopup <- function(dateTime, location,dependency=NULL){
  date_time=get_dateTime(dateTime)
  require(htmltools)
  tagList(tag_geoPopup(date=date_time$date, time=date_time$time, location), geoPopup_dependency(), dependency)
}
get_dateTime <- function(D) {
  list(
    date=lubridate::date(D),
    time=paste0(lubridate::hour(D),"點",
      lubridate::minute(D),"分"))
}
get_location= function(trafficX){
  paste0(trafficX$緯度[[1]],",",trafficX$經度[[1]])
}
tag_mappin = function(){
  tags$img(src="lib/econIDV-1.0.0/img/maplocation.svg", style="position: absolute;
width: 15px;
height: 15px;
left: 4.5px;
top: 4.5px;

/* background: #000000; */")
}
tag_trafficPopup <- function(trafficX=traffic[1,]) {
  ui_geoPopup(
    dateTime=trafficX$發生時間[[1]],
    location=get_location(trafficX)
  ) #|> econWeb::browseTag2()
}

