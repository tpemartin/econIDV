create_traceData = function() {
  data.frame(
    x=1:5,
    y=sample(1:5, 5, T)
  )}

create_data = function(){
  set.seed(2938)
  .GlobalEnv$trace1 = create_traceData()
  .GlobalEnv$trace2 = create_traceData()
}
plot_trace = function(){
  plotly::plot_ly() |>
    add_trace(
      data=trace1,
      type="scatter", mode="lines",
      x=~x, y=~y
    ) |>
    add_trace(
      data=trace2,
      type="scatter", mode="lines",
      x=~x, y=~y
    ) |>
    plotly::layout(
      title=list(text="Example")
    )
}