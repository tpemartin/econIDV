
factor(
  dataFrame_byHour_byCounty$名次分類,
  levels=c("第一名","第二名","第三名","其餘名次")) ->
  dataFrame_byHour_byCounty$名次分類

dataFrame_byHour_byCounty |> names()
plot_frequency = function(data){
  plotly::plot_ly() |>
    add_trace(
      data=data,
      type="bar",
      x=~時段,
      y=~車禍次數,
      color=~名次分類,
      colors=c("#ff1616","#f07b3f","#ffd460",scales::alpha("#a8cff5", 0.7)),
      hoverinfo="text"
    ) |>
    plotly::layout(barmode="stack", showlegend=F)
}
plot_share <- function(dataFrame_byHour_byCounty2) {
  dataFrame_byHour_byCounty2 |>
    plotly::plot_ly() |>
    add_trace(
      data=data,
      type="bar",
      x=~時段,
      y=~車禍佔比,
      color=~名次分類,
      colors=c("#ff1616","#f07b3f","#ffd460",scales::alpha("#a8cff5", 0.7)),
      text=~stringr::str_glue("{名次分類}:{format(車禍佔比, digits=2, nsmall=2)}"),
      hoverinfo="text"
    ) |>
    plotly::layout(barmode="stack", showlegend=F)
}

