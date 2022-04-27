
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

