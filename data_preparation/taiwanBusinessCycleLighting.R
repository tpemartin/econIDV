
# googlesheets4::read_sheet(
#   ss="https://docs.google.com/spreadsheets/d/1EpFR4ZXswrygsSVAHegCL4ht1wRyJWT9-Ur2VOp1CRA/edit?usp=sharing"
# ) -> taiwanBusinessCycle
#
# dataEnv=new.env()
build_data <- function(bindingObjectName){
  build_data_callerEnv=rlang::caller_env()
  require(promises)
  taiwanBusinessCycle <- future_promise(googlesheets4::read_sheet(
    ss="https://docs.google.com/spreadsheets/d/1OxQrBiAoxaRWY_E-AoRw894v7vdB-DMuiGH1S8oOm_o/edit#gid=354140395"
  ))

  taiwanBusinessCycle |>
  then(
      onFulfilled = cleanup_taiwanBusinessCycle(build_data_callerEnv, bindingObjectName)
    )
}
cleanup_taiwanBusinessCycle <- function(returnEnv, bindingObjectName){
  function(taiwanBusinessCycle) {
    library(tidyverse)
    taiwanBusinessCycle |>
      slice(-1) -> taiwanBusinessCycle
    taiwanBusinessCycle |>
      mutate(
        across(
          .cols=-1,
          .funs = unlist
        )
      ) |>
      rename(
        "date"="...1") |>
      mutate(
        date=lubridate::ym(date)
      )-> df0
    df0$`景氣對策信號(分)` |> purrr::map(~ifelse(is.null(.x), NA, .x)) |> unlist() |> as.integer() ->
      df0$`景氣對策信號(分)`
    df0 |>
      mutate(
        across(
          .cols=-1,
          .fns=as.integer
        )
      ) -> df0
    df0$`景氣對策信號(分)` |>
      cut(
        c(8,16.5, 22.5, 31.5, 37.5, 45)
      ) -> light
    levels(light) <-
      c("低迷", "轉向上", "穩定","轉向下","熱絡")
    light -> df0$`景氣對策信號(燈號)`

    df0$`景氣對策信號(燈號)` ->
      df0$type -> df0$startColor -> df0$endColor
    levels(df0$type) <- c("none", "vertical", "none", "vertical", "none")
    levels(df0$startColor) <- c("#2f2dd0", "#f3ed13", "#dafddd", "#db2205", "#fd1e1c")
    levels(df0$endColor) <- c("", "#0c09ec", "", "#fef906", "")
    df0$strokeColor <- "#cecece"
    df0$strokeWidth <- 2
    df0 -> df_businessCycleLighting
    assign(bindingObjectName, df_businessCycleLighting, envir=returnEnv)
    # df_businessCycleLighting
  }
}
drop_json = teachDS::drop_jsonFunctional("/Users/martinl/Dropbox/github-data")
# df_businessCycleLighting |> jsonlite::toJSON()
# drop_json(df_businessCycleLighting)
# require(promises)
# future_promise(googlesheets4::read_sheet(
#   ss="https://docs.google.com/spreadsheets/d/1OxQrBiAoxaRWY_E-AoRw894v7vdB-DMuiGH1S8oOm_o/edit#gid=354140395"
# )) %...>%
#   cleanup_taiwanBusinessCycle %...>%
#   (function(df_businessCycleLighting){
#     .GlobalEnv$df_businessCycleLighting
#   })
#   # (function(df_businessCycleLighting){
#   #   drop_json(df_businessCycleLighting)
#   #   } )
# drop_json(df_businessCycleLighting)
build_data("df_businessCycleLighting")
drop_json(df_businessCycleLighting)

{
  jsonlite::fromJSON("https://www.dropbox.com/s/pqah01wcv8vuwo2/df_businessCycleLighting.json?dl=1") -> df_businessCycleLighting
  require(tidyverse)
  df_businessCycleLighting |>
    select(
      date, contains("分"), contains("不含趨勢")
    ) |>
    na.omit() |>
    mutate(
      date=lubridate::ymd(date))-> df_indicators

df_indicators  |> View()
  View(df_businessCycleLighting)
df_indicators |>
  tidyr::pivot_longer(
    cols=contains("不含趨勢"),
    names_to = "type",
    values_to = "index"
  ) -> df_indicatorsLong
View(df_indicatorsLong)
ggplot(data=df_indicators, aes(x=date))+
    geom_line(
      aes(y=`景氣對策信號(分)`)
    ) +
    geom_line(
      data=df_indicatorsLong,
      aes(y=index, color=type)
    )
require(plotly)
fig <- plot_ly() |>
  add_lines(
    data=df_indicators,
    x= ~date,
    y= ~`景氣對策信號(分)`,
    name="景氣對策信號(分)"
  )
fig2 <- plot_ly() |>
  add_lines(
    data=df_indicatorsLong,
    x= ~date,
    y= ~index,
    color = ~type,
    yaxis="y2",
    opacity=0.5
  )
subplot(
  list(fig, fig2),
  nrows = 2,
  shareX= TRUE
)
scatter
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "<b>secondary</b> yaxis title")

  fig |>
    layout(
      yaxis2=list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "<b>secondary</b> yaxis title",
        range = list(60,120))
    )
}
