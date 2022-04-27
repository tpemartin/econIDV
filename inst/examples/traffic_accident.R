download_traffic_data=function(){
  json <- jsonlite::fromJSON("https://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=01987403-8634-4E3F-B626-E7777014AE43")
  # json$result$records |> View()

  traffic <- json$result$records
  traffic |>
    DT::datatable()
  traffic$發生時間 |>
    stringr::str_split("年") |> #View()
    purrr::map_chr(
      ~{
        as.integer(.x[[1]])+1911 -> .year
        paste0(.year,"-",.x[[2]])
      }
    ) -> .dateTime
  lubridate::ymd_hms(.dateTime, tz="Asia/Taipei") ->
    traffic$發生時間

  purrr::map(
    list(
      lubridate::hour,
      lubridate::minute
    ),
    ~{
      .x(traffic$發生時間)
    }
  ) |>
    setNames(c("hour", "minute"))-> list_hm
  traffic$hour <- list_hm$hour
  traffic$minute <- list_hm$minute

  traffic$發生地點 |>
    stringr::str_extract(
      "^[^市]{2}市.{1,3}[區鄉鎮](?!區)"
    ) -> .city1
  traffic$發生地點 |>
    stringr::str_extract(
      "^[^縣]{2}[縣].{1,3}[區鄉鎮市](?!區)"
    ) -> .city2
  dplyr::if_else(
    is.na(.city1), .city2, .city1
  ) -> .city_town
  traffic$縣市 = stringr::str_sub(.city_town, 1,3)
  traffic$區鄉鎮市 = stringr::str_sub(.city_town, 4)

  glue::glue("https://www.google.com/maps/search/?api=1&query={traffic$緯度},{traffic$經度}") ->
    traffic$googleMap

  traffic -> .GlobalEnv$traffic
  message("Data frame traffic is created in the global environment.")
}



# library(leaflet)
# econDV2::Map() -> map
# map$copy_paste$osm_bbox()
# bbox=c(left=119.325,
#   bottom=21.404,
#   right=122.649,
#   top=25.594)

map_traffic <- function(trafficData) {
  library(leaflet)
  leaflet(data=trafficData, height="700px") |>
    setView(
      lat=23.8389,
      lng = 120.6217,
      zoom=8
    ) |>
    addTiles() |>
    addMarkers(
      lng=~as.numeric(經度),
      lat=~as.numeric(緯度)#,
      # clusterOptions = markerClusterOptions()
    )
} #|> econIDV::showWidget()
map_traffic2 <- function(trafficData) {
  library(leaflet)
  leaflet(data=trafficData) |>
    setView(
      lat=23.8389,
      lng = 120.6217,
      zoom=7
    ) |>
    addTiles() |>
    addMarkers(
      lng=~as.numeric(經度),
      lat=~as.numeric(緯度),
      clusterOptions = markerClusterOptions()
    )
} #|> econIDV::showWidget()

map_traffic <- function(trafficData) {
  library(leaflet)
  leaflet(data=trafficData, height="700px") |>
    setView(
      lat=23.8389,
      lng = 120.6217,
      zoom=8
    ) |>
    addTiles() |>
    addMarkers(
      lng=~as.numeric(經度),
      lat=~as.numeric(緯度)#,
      # clusterOptions = markerClusterOptions()
    )
} #|> econIDV::showWidget()
dayNight_iconMarkers <- function(){
  require(leaflet)
  dayNightIcons <- iconList(
    day = makeIcon(
      iconUrl=system.file("html/img/sun.svg",package="econIDV"), iconWidth=17, iconHeight=17),
    night = makeIcon(
      iconUrl=system.file("html/img/moon.svg",package="econIDV"), iconWidth=17, iconHeight=17)
  )
}
add_dayNight <- function(trafficData) {
  pickDay <- (trafficData$hour >= 6 &
      trafficData$hour < 18)
  dayNight=dplyr::if_else(
    pickDay,
    "day","night"
  )
}
create_iconType <- function(trafficData) {
  trafficData$dayNight = add_dayNight(trafficData)
  trafficData$iconType=
    dayNight_iconMarkers()[trafficData$dayNight]
  trafficData
}
add_popupText = function(traffic){
  traffic$popup = purrr::map_chr(
    1:nrow(traffic),
    ~{
      traffic[.x, ] |>
        get_trafficPopupAsString()
    }
  )
  return(traffic)
}
get_trafficPopupAsString = function(trafficX){
  tag_trafficPopup(trafficX) |> as.character()
}
map_traffic_withDayNight <- function(trafficData) {
  library(leaflet)
  # trafficData=traffic

  leaflet(data=trafficData) |>
    setView(
      lat=23.8389,
      lng = 120.6217,
      zoom=7
    ) |>
    addTiles() |>
    addMarkers(
      lng=~as.numeric(經度),
      lat=~as.numeric(緯度),
      icon=~iconType,
      popup =~popup
    ) |>
    htmltools::tagList(
      geoPopup_dependency()
    )
}
show_datatable <- function(traffic){
  DT::datatable(
    traffic
  )
}

dashboard_traffic <- function( ..., filterWidget=NULL){
  require(shinydashboard)
  shinydashboard::dashboardPage(
    dashboardHeader(title="交通事故"),
    dashboardSidebar(filterWidget),
    dashboardBody(...)

  )
}

plot_histogram = function(traffic){
  require(ggplot2)
  require(plotly)

  ggplot(traffic)+
    geom_bar(
      aes(x=hour,
        group=縣市),
      fill="dodgerblue"
    ) +
    theme_classic()-> gg0
    ggplotly(gg0)
}

traffic_example = function(){
  .GlobalEnv$examples=Examples()
  examples$traffic_accident()
  examples$traffic_accident_env$attach()
}
