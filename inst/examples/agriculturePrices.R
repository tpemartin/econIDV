# ex <- new.env()
download_agriculturePrices <- function(){
  xml2::read_xml("https://www.dgbas.gov.tw/public/data/open/stat/price/PR0101A3M.xml") ->
    xml
  xml |> prepare_agProductDataFrame() ->
    .GlobalEnv$df_agPrices
  message("df_agPrices is in the global environment now.")
}
get_featureValues <- function(xml, xpath="//Item") {
  xml |>
    xml2::xml_find_all(xpath) |>
    xml2::xml_text() #-> priceTypes
}
get_agProductObs <- function(productTypes, allObs, whichIsOriginalValue) {
  # whichIsTarget = which(priceTypes == "一.食物類(民國105年=100)")
  # whichIsNextTarget = which(priceTypes == "二.衣著類(民國105年=100)")
  productTypes |> as.character() |> unique() -> xx
  xx |> stringr::str_which("二.衣著") -> nonAgloc
  xx[1:(nonAgloc-1)] -> xx
  xx |> stringr::str_which("^[0-9]{1,3}\\.", negate = T) -> nonCats
  xx[nonCats] |>
    stringr::str_subset("^[0-9]") -> agItems
  agProductsLocs = which(productTypes %in% agItems)
  # priceTypes |>
    # stringr::str_which("^[0-9]{2}(?![[0-9]\\.])") -> agProductsLocs

  # priceTypes[(whichIsTarget[[2]]:(whichIsNextTarget[[1]]-1))] |>
  #   stringr::str_which("^[0-9]{2}(?![[0-9]\\.])") -> whichAreAgricultureProducts
  # agProductsLocs <- (4:398)[whichAreAgricultureProducts]
  agProductsLocs <- intersect(agProductsLocs, whichIsOriginalValue)
  allObs[agProductsLocs]
}
get_dataFrame <- function(agObsX) {
  list() -> list0
  for(.x in 1:(xml2::xml_length(agObsX))){
    agObsX |>
      xml2::xml_child(.x) -> featureX
    featureX  |>
      xml2::xml_name() -> vname
    featureX |>
      xml2::xml_text() -> vvalue
    list0[[vname]] = vvalue
  }
  list0 |> list2DF()
}
prepare_agProductDataFrame <- get_agProductDataFrame <- function(xml) {
  xml |>
    xml2::xml_children() -> allObs
  productTypes = get_featureValues(xml) |> factor()
  valueTypes = get_featureValues(xml, "//TYPE") |> factor()
  whichIsOriginalValue = which(
    valueTypes=="原始值")

  agObs = get_agProductObs(productTypes, allObs, whichIsOriginalValue)

  purrr::map_dfr(
    agObs, get_dataFrame
  ) -> df_agPrices

  df_agPrices$Item_VALUE |>
    as.numeric() -> df_agPrices$Item_VALUE
  df_agPrices$Item |>
    factor() -> df_agPrices$Item
  df_agPrices$TIME_PERIOD |> paste0("D01") |> lubridate::ymd() ->
    df_agPrices$TIME_PERIOD

  df_agPrices$Item |>
    stringr::str_extract(
      # "19其他肉類製品(火腿、臘肉、肉類罐頭等)(民國105年=100)",
      "(?<=[0-9]{1,3})[^0-9]*(?=\\(民國)"
    ) -> df_agPrices$Item2

  df_agPrices |>
    dplyr::mutate(
      month=lubridate::month(TIME_PERIOD),
      year=lubridate::year(TIME_PERIOD),
      text=glue::glue("{Item2}, {month}月")
    ) -> df_agPrices
  df_agPrices
}
# xml |> get_agProductDataFrame() ->
#   df_agPrices
add_categories <- function(df_agPrices) {

  df_agPrices$Item |> levels() -> itemLevels
  stringr::str_extract(itemLevels,"^[0-9]+") |>
    as.numeric() -> itemNumbers

  itemLevels[
    order(itemNumbers)
  ] -> itemLevels_ordered

  df_agPrices$Item <-
    factor(
      df_agPrices$Item,
      levels=itemLevels_ordered
    )
  df_agPrices$Item -> df_agPrices$Cat
  # levels(df_agPrices$Cat) -> catLevels
  catCut <- c(0, 10, # 米、麵、麥
    16, # 肉
    19, # 肉製品
    21, # 蛋
    36, # 魚
    39, # 甲殼類
    41, # 其他海鮮
    59, # 根莖類
    72, # 葉菜類
    88, # 其他
    94, # 加工蔬菜
    118, # 水果
    120, # 加工水果
    121, # 鮮奶
    124, # 乳製品
    127, # 油
    132, # 調味品
    136, # 酒
    138, # 茶葉相關
    146, # 飲料
    152, # 調理食品
    164, # 餐廳料理
    171 # 休閒食品
  )
  categories = c("米、麵、麥", "肉", "肉製品", "蛋", "魚", "甲殼類",
    "其他海鮮", "根莖類", "葉菜類", "其他", "加工蔬菜",
    "水果", "加工水果", "鮮奶", "乳製品", "油", "調味品",
    "酒", "茶葉相關", "飲料", "調理食品", "餐廳料理",
    "休閒食品")
  1:171 |>
    cut(catCut) -> afterCut
  levels(afterCut) <- categories
  levels(df_agPrices$Cat) <- as.character(afterCut)
  df_agPrices
}
plot_highlighted <- function(df_highlighted){
 df_highlighted |>
    plotly::plot_ly() |>
    add_trace(
      type="scatter", mode="lines",
      x=~TIME_PERIOD,
      y=~Item_VALUE,
      name=~Item2,
      split=~Item2,
      color=I("#828282"),
      legendgroup=~Cat,
      hoverinfo='y+x+name',
      line=list(width=0.1)
    ) |>
    plotly::layout(
      legend=list(
        orientation="h"
      ),
      yaxis=list(showgrid=F, showline=F, title=list(text=NULL)),
      xaxis=list(showgrid=F, title=list(text=NULL))
    )
}
plot_agprices_complete <- function(df) {
  xgrids <- list(
    gridwidth = 0.1,
    gridcolor = "#434343",
    showgrid = T, showline = T
  )
  xticks <- list(
    dtick = "M12", # 座標點相隔12個月
    tickformat = "%Y", # 座標只放年
    ticklabelmode = "period"
  )
  xrangeSelector <- list(
    rangeselector = {
      list(
        buttons = {
          list(
            list(
              count = 3,
              label = "3 mo",
              step = "month",
              stepmode = "backward"
            ),
            list(
              count = 6,
              label = "6 mo",
              step = "month",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "YTD",
              step = "year",
              stepmode = "todate"
            ),
            list(step = "all")
          )
        }
      )
    }
  )
  xrangeSlider <- list(
    rangeslider = list(type = "date")
  )

  plotly::plot_ly(df) |>
    add_trace(
      type = "scatter", mode = "lines",
      x = ~TIME_PERIOD,
      y = ~Item_VALUE,
      name = ~Item2, # trace名稱, 也是legend label
      split = ~Item2, # 切割trace依據
      color = I("white"),
      legendgroup = ~Cat, # 相同Cat合併成一trace.
      # legendgrouptitle=list(font=list(color="white")),
      hovertemplate = "%{x|%b%Y}",
      line = list(width = 0.3)
    ) |>
    plotly::layout(
      title=list(text="各類食品消費者物價指數",
        font=list(color="white")),
      legend = list(
        orientation = "h",
        y=-0.2,
        font=list(color="#e6e6e6"),
        grouptitlefont=list(color="white"),
        title=list(font=list(color="white"))
      ),
      height=600,
      plot_bgcolor = "black",
      paper_bgcolor = "#c5a880",
      margin=list(t=57),
      yaxis = list(
        gridwidth = 0.1, gridcolor = "#434343",
        showgrid = T, showline = F,
        title = list(text = NULL),
        range = c(0, 250)
      ), # fixed range
      xaxis =
        c(
          list(title=list(text = NULL)),
          xgrids,
          xticks,
          # Range slider and selectors
          xrangeSelector,
          xrangeSlider
        )
    ) |>
    econIDV::add_legendgrouptitle(font=list(color="white")) -> plt_complete
  plt_complete
}
