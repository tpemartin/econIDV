download_electionData <- function(){
  .GlobalEnv$elections = jsonlite::fromJSON("https://www.dropbox.com/s/ygydokt82wmsk0d/elections.json?dl=1")
  message("Object elections is in the global environment now.")
}
prepare_wider_data <- function(df_elections, .select=c("地區","年","得票率顏色")) {
  df_elections |>
    select(.select) |>
    tidyr::pivot_wider(
      names_prefix = "y",
      names_from=.select[[2]],
      values_from=.select[[3]]
    ) -> df_elections_wider
  df_elections_wider
}
plot_electionsOverYears <- function(sf_elections_wider) {
  ggplot(data=sf_elections_wider)+theme_void() -> base
  # as.is operator I(.) does not work as fill want, it would be come numeric unless as.character in advance.
  list_gg=vector("list", 3)
  base+
    geom_sf(
      aes(fill=I(as.character(y2012))),
      color="#828282", size=0.1) -> list_gg[[1]]
  base+
    geom_sf(
      aes(fill=I(as.character(y2016))),
      color="#828282", size=0.1) -> list_gg[[2]]
  base+
    geom_sf(
      aes(fill=I(as.character(y2020))),
      color="#828282", size=0.1) -> list_gg[[3]]

  purrr::map(
    list_gg,
    ~plotly::ggplotly(.x)
  ) ->
    list_ggplt
  list_gg
}
create_subplot <- function(list_ggplt) {
  do.call(plotly::subplot, list_ggplt) -> plt0

  plt0 |>
    plotly::layout(
      showlegend=F
    )
}
clean_merge_data <- function(elections) {
  elections |>
    purrr::map(clean_data) -> elections2

  elections2$e13$年 <- 2012
  elections2$e13$政黨 <- elections2$e13$姓名
  levels(elections2$e13$政黨) <- c("親民黨","民進黨","國民黨")
  elections2$e14$年 <- 2016
  elections2$e14$政黨 <- elections2$e14$姓名
  levels(elections2$e14$政黨) <- c("親民黨","國民黨","民進黨")
  elections2$e15$年 <- 2020
  elections2$e15$政黨 <- elections2$e15$姓名
  levels(elections2$e15$政黨) <- c("親民黨","民進黨","國民黨")

  elections2 |>
    purrr::reduce(dplyr::bind_rows) -> elections3
  elections3[elections3$地區=="桃園縣",]$地區 = "桃園市"
  elections3

}
directPlotlyPlot <- function(sf_elections_wider){
  list_plt = vector("list", 3 )
  plotly::plot_ly() |>
    plotly::add_sf(
      name="2012",
      data=sf_elections_wider,
      split=~地區,
      color=~I(as.character(y2012)),
      alpha=1
    ) |>
    plotly::layout(
      xaxis=list(
        title=list(
          text="2012"
        ),
        side="top"
      )
    )-> list_plt[[1]]
  plotly::plot_ly() |>
    plotly::add_sf(
      data=sf_elections_wider,
      split=~地區,
      color=~I(as.character(y2016)),
      alpha=1
    ) |>
    plotly::layout(
      xaxis=list(
        title=list(
          text="2016"
        ),
        side="top"
      )
    ) -> list_plt[[2]]
  plotly::plot_ly() |>
    plotly::add_sf(
      data=sf_elections_wider,
      split=~地區,
      color=~I(as.character(y2020)),
      alpha=1
    ) |>
    plotly::layout(
      xaxis=list(
        title=list(
          text="2020"
        ),
        side="top"
      )
    ) -> list_plt[[3]]

  plotly::subplot(
    list_plt[[1]], list_plt[[2]], list_plt[[3]],
    titleX = T
  ) |>
    plotly::style(
      line=list(
        width=0.2,
        color="#828282"
      )
    ) |>
    plotly::layout(
      showlegend=F
    ) |>
    config(
      displayModeBar = FALSE
    )
}
prepare_dataTableData <- function(df_elections) {
  df_elections |>
    mutate(
      兩黨得票率=glue::glue(
        "<div>民進黨:{民進黨}<br>國民黨:{國民黨}</div>"
      )
    ) -> df_4dataTable
  # you can browse html effect through:
  # browsable(HTML("<div>民進黨:{民進黨}<br>國民黨:{國民黨}</div>"))

  df_4dataTable |>
    select(
      地區, 年, 兩黨得票率
    ) |>
    tidyr::pivot_wider(
      names_from="年",
      values_from="兩黨得票率"
    ) -> df_4dataTable_wide

  df_4dataTable_wide
}
prepare_choroplethMapData <- function(elections) {
  elections |>
    clean_merge_data() |>
    prepare_for_choroplethColor() ->
    df_elections
  df_elections
}
prepare_fillColorByYears_data <- function(df_elections){
  df_elections |>
    select(c("地區","年","得票率顏色")) |>
    tidyr::pivot_wider(
      names_prefix = "y",
      names_from="年",
      values_from="得票率顏色"
    ) -> df_elections_wider
  df_elections_wider
}
prepare_for_choroplethColor <- function(df_elections) {
  df_elections |>
    dplyr::filter(
      政黨 %in% c("國民黨","民進黨")
    ) -> df_elections

  df_elections |>
    dplyr::filter(
      政黨 %in% c("國民黨","民進黨")
    ) -> df_elections

  df_elections |>
    dplyr::select(年, 地區, 政黨, 得票率) |>
    tidyr::pivot_wider(
      names_from="政黨",
      values_from="得票率"
    ) |>
    mutate(
      勝出政黨=dplyr::if_else(國民黨>民進黨,"國民黨","民進黨"),
      勝出政黨得票率=pmax(國民黨, 民進黨),
      取色得票率=dplyr::if_else(國民黨>民進黨,-勝出政黨得票率,勝出政黨得票率)
    ) -> df_elections2

  df_elections2$取色得票率 |>
    quantile(c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

  levelCuts =
    c(0, 50, 55, 60, 65, 100)
  levelCuts = c(-levelCuts, levelCuts) |> unique()

  df_elections2$取色得票率區間 <-
    df_elections2$取色得票率 |>
    cut(levelCuts)

  nLevels=length(levels(df_elections2$取色得票率區間))

  pal = generate_palette()

  df_elections2$得票率顏色=df_elections2$取色得票率區間
  levels(df_elections2$得票率顏色) <-
    pal

  df_elections2
}
create_sf <- function(df_elections) {
  df_elections2=df_elections
  econDV2::Map() -> map
  map$sf$get_sf_taiwan_simplified() -> sf_taiwan_simplified

  sf_taiwan_simplified$`台灣本島`$縣市 -> sf

  sf |>
    dplyr::filter(
      name=="桃園市"
    ) -> sf_taoyuan
  sf_taoyuan$name <-
    sf_taoyuan$map_id <- "桃園縣"
  dplyr::bind_rows(
    sf,
    sf_taoyuan
  ) -> sf

  df_elections2 |>
    left_join(
      sf,
      by=c(地區="map_id")
    ) ->
    sf_elections

  sf_elections |>
    sf::st_set_geometry(
      sf_elections$geometry
    ) -> sf_elections
  sf_elections
}
generate_palette <- function() {
  colorspace::diverging_hcl(n = 10, h = c(245, 120), c = c(31, 100), l = c(30, 100), power = c(1, 1.3), register = "kmt-dpp") -> pal
  pal
}
clean_data <- function(.df) {
  .df[1,] |> as.character() -> vnames
  names(.df) <- vnames
  library(dplyr)
  .df |>
    slice(-1) -> .df2
  .df2$得票數 |> as.numeric() -> .df2$得票數
  .df2$得票率 |>
    stringr::str_remove("%") |>
    as.numeric() -> .df2$得票率
  .df2$姓名 |> factor() -> .df2$姓名
  .df2$號次 |> factor() -> .df2$號次
  .df2
}
plot_electionMaps <- function(sf_elections) {
  ggplot()+theme_void() -> base
  purrr::map(
    seq(2012, 2020, 4),
    ~{
      sf_elections |>
        dplyr::filter(年==.x) -> sfX
      base+geom_sf(data=sfX,
        aes(
          fill=I(得票率顏色)
        ),
        color="#828282", size=0.1)
    }
  ) -> list_gg
  list_gg
}
plot_oneYearElectionMap = function(sfX){
  ggplot(data=sfX)+theme_void() -> base
  base+geom_sf(
    aes(
      fill=I(得票率顏色)
    ),
    color="#828282", size=0.1)
}
add_intervalLabels <- function(sf_elections) {
  sf_elections$得票率顏色說明=sf_elections$得票率顏色
  sf_elections$取色得票率區間 |>
    levels() -> intervals
  stringr::str_extract_all(intervals[6:10], "[:digit:]+") |>
    purrr::map_chr(
      ~{paste0(.x, collapse = "-")}
    ) -> intervals2
  levels(sf_elections$得票率顏色說明) <- c(rev(intervals2), intervals2)
  sf_elections
}
create_distinctLabels <- function(.interval_part) {
  .label = .interval_part
  levels(.label) -> labelLevels
  labelLevels
  labelLevels[-c(2,7)] |> stringr::str_extract("[[:digit:]-]+") ->
    labelLevels[-c(2,7)]

  # labelLevels[7:10] <- paste0(labelLevels[7:10], "<span></span> ")
  labelLevels[c(2,7)] <- c('國民黨<br>50-55',"民進黨<br>50-55")
  labelLevels[7:10] <- paste0(labelLevels[7:10], "<span></span> ")
  labelLevels
}
get_orderedLabels <- function(sf_election2012){
  levels(sf_election2012$interval_part)
  levels(sf_election2012$得票率顏色說明) |> rev() -> intervalLevels_ordered
  parties <- rep(c("國民黨","民進黨"), each=5)

  labels_ordered <- paste(intervalLevels_ordered,parties,sep=":")
  labels_ordered
}
create_newLevels4split <- function(df_elections) {
  newSplit = df_elections$取色得票率區間
  levels(df_elections$取色得票率區間)[6:10] |>
    stringr::str_extract_all("[[:digit:],]+") |>
    purrr::map_chr(~stringr::str_replace(.x, ",", "-")) -> halfLevels
  newLevels <-
    c(paste0(rev(halfLevels),"<span></span>"), halfLevels)
  levels(newSplit) = newLevels

  factor(
    newSplit,
    levels=c(paste0(halfLevels, "<span></span>"),halfLevels)
  ) -> newSplit

  newSplit
}
plot_allYears <- function(sf_electionsByYears, ...) {
  rlang::enexprs(...) -> quo_dot3
  rlang::exprs(
    data=sf_electionsByYears[[.x]],
    color=~I(as.character(得票率顏色)),
    alpha=1
  ) -> quo_others
  c(quo_dot3, quo_others) -> quo_all

  list_plts <- vector("list",3)
  for(.x in 1:3){
    rlang::expr(
      plotly::plot_ly() |>
        add_sf(
          !!!quo_all
        )
    ) -> toDo
    rlang::eval_bare(toDo)-> list_plts[[.x]]
  }
  list_plts
}
get_twoPartyPals <- function(){
  pal=generate_palette()
  c(rev(pal[1:5]), pal[6:10])
}

add_adjustedWinningPartyVoteShare <- function(sf_electionX) {
  .intervals =
    levels(sf_electionX$勝黨得票率區間)

  .intervals[c(
    seq(9,1,-2),
    seq(10,2, -2)
  )] -> .intervals2

  # Change levels order
  sf_electionX$勝黨得票率區間2 <-
    factor(
      sf_electionX$勝黨得票率區間,
      levels=.intervals2
    )
  sf_electionX
}

add_trace_name <- function(sf_electionX) {
  sf_electionX$trace_name =
    sf_electionX$勝黨得票率區間2
  levels(sf_electionX$trace_name)
  oldLevels =
    levels(sf_electionX$trace_name)

  oldLevels |>
    stringr::str_sub(end = -5) -> newLevels
  newLevels
  newLevels ->
    levels(sf_electionX$trace_name)
  sf_electionX$trace_name
  sf_electionX
}
generate_plots_countiesXparties <- function(sf_electionsByYears) {
  twoPartyPals = get_twoPartyPals()
  list_splitByCountiesGroupByParties <- vector("list", 3)
  for(.x in 1:3){
    plotly::plot_ly() |>
      add_sf(
        data=sf_electionsByYears[[.x]],
        legendgroup=~勝出政黨,
        name=~paste0(round(勝出政黨得票率,digits=2)), #factor(勝出政黨得票率):factor(地區),
        split=~地區,
        color=~勝黨得票率區間2, colors=twoPartyPals,
        alpha=1
      ) -> list_splitByCountiesGroupByParties[[.x]]
  }
  list_splitByCountiesGroupByParties
}

generate_subplot_countiesXparties <- function( list_splitByCountiesGroupByParties) {
  plotly::subplot(list_splitByCountiesGroupByParties) ->
    subplot_splitByCountiesGroupByParties
  subplot_splitByCountiesGroupByParties |>
    plotly::style(
      legendgrouptitle=list(text="國民黨"),
      traces=1
    ) |>
    plotly::style(
      legendgrouptitle=list(text="民進黨"),
      traces=2
    ) |>
    plotly::layout(
      legend=list(
        # title=list(
        #   text="得票率"
        # ),
        orientation="h"
      )
    ) -> subplot_splitByCountiesGroupByParties
  subplot_splitByCountiesGroupByParties
}
plot_3yearsElection <- function( sf_electionsByYears) {
  list_splitByCounties <- vector("list", 3)
  for(.x in 1:3){
    plotly::plot_ly() |>
      add_sf(
        data=sf_electionsByYears[[.x]],
        legendgroup=~地區,
        name=~label_share(勝出政黨得票率),
        color=~勝黨得票率區間2,
        colors=twoPartyPals,
        alpha=1
      ) -> list_splitByCounties[[.x]]
  }

  plotly::subplot(list_splitByCounties) -> subplot0
  subplot0
}
