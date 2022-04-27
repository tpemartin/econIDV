examples=Examples()
examples$traffic_accident()
examples$traffic_accident_env$attach()
library(tidyverse)
library(leaflet)
download_traffic_data()

# create leaflet_icon_set for day and night
dayNightIcons <- iconList(
  day = makeIcon(
    iconUrl=system.file("html/img/sun.svg",package="econIDV"),
    iconWidth=17,
    iconHeight=17),
  night = makeIcon(
    iconUrl=system.file("html/img/moon.svg",package="econIDV"),
    iconWidth=17,
    iconHeight=17)
)
# Assign icons to each observation:
traffic$dayNight = add_dayNight(traffic)
traffic$iconType = dayNightIcons[traffic$dayNight]

# add html tag for popups as in traffic$popup
traffic = add_popupText(traffic)

leaflet(data=traffic) |>
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
    popup = ~popup
  )



download_traffic_data()

# create leaflet_icon_set for day and night
dayNightIcons <- iconList(
  day = makeIcon(
    iconUrl=system.file("html/img/sun.svg",package="econIDV"),
    iconWidth=17,
    iconHeight=17),
  night = makeIcon(
    iconUrl=system.file("html/img/moon.svg",package="econIDV"),
    iconWidth=17,
    iconHeight=17)
)
# Add day/night and popup info
traffic$dayNight = add_dayNight(traffic)
traffic$iconType = dayNightIcons[traffic$dayNight]

leaflet(data=traffic) |>
  setView(
    lat=23.8389,
    lng = 120.6217,
    zoom=7
  ) |>
  addTiles() |>
  addMarkers(
    lng=~as.numeric(經度),
    lat=~as.numeric(緯度),
    icon=~iconType
  )
traffic = create_iconType(traffic)
traffic = add_popupText(traffic)
undebug(map_traffic_withDayNight)
map_traffic_withDayNight(traffic) |>
  econIDV::showWidget()

# create sharedData object
trafficShare <- crosstalk::SharedData$new(traffic, group="traffic")

# create a smaller sharedData object from the same source
traffic |>
  select(c("發生時間", "發生地點", "死亡受傷人數", "車種", "hour", "縣市","區鄉鎮市")) -> trafficPartial
trafficSharePartial <- crosstalk::SharedData$new(trafficPartial, group = "traffic")


dashboard_traffic(
  crosstalk::bscols(
    widths = c(4,8),
    shinydashboard::box(
      width=12,
      map_traffic_withDayNight(trafficShare)
    ),
    shinydashboard::box(
      width=12,
      DT::datatable(trafficSharePartial ,
        filter = "top"))
  ) ,
  filterWidget=
    shiny::fluidRow(
      shiny::column(
        width=10, offset=1,
        crosstalk::filter_checkbox(
          id="hour-filter",
          label="時",
          sharedData =  trafficShare,
          group=~hour,
          inline = T
        )
      ),
      shiny::column(
        width=10, offset=1,
        crosstalk::filter_select(
          "dayNight-filter",
          label="白天/夜間",
          sharedData = trafficShare,
          group=~dayNight
        )
      ),
      shiny::column(
        width=10, offset=1,
        crosstalk::filter_select(
          "city-filter",
          label="縣市",
          sharedData = trafficShare,
          group=~縣市
        )
      )
    )
) -> a1accident

a1accident |>
  econIDV::showWidget()

a1accident |>
  econIDV::prepare_githubPage() |>
  htmltools::save_html( "/Users/martinl/Github/econIDV/docs/a1accident2.html")
