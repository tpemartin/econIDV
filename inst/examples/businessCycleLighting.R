Example1 <- function(){
  ex <- new.env()
  ex$businessCycleLighting <- list()
  ex$businessCycleLighting$download <- function(overwrite=T){
    download_df_businessCycleLighting(overwrite)
    ex$businessCycleLighting$plot <- function(){
      ex$businessCycleLighting$save <- function() {
        ex$businessCycleLighting$load <- load_plt_businessCycle
        save_plt_businessCycle()
      }

      plt_businessCycle()
    }

  }
  return(ex)
}
download_df_businessCycleLighting <- function(overwrite=F){

  if(!exists("df_businessCycleLighting", envir=.GlobalEnv) || overwrite){
    jsonlite::fromJSON("https://www.dropbox.com/s/pqah01wcv8vuwo2/df_businessCycleLighting.json?dl=1") -> dfx
    dfx$date |> lubridate::ymd() -> dfx$date
    dfx ->
      .GlobalEnv$df_businessCycleLighting
    message("Object df_businessCycleLighting is created in the global environment")
  }

}

plt_businessCycle <- function(){
  require(plotly)
  plot_ly(data=df_businessCycleLighting) |>
    add_lines(
      x=~date,
      y=~`景氣對策信號(分)`
    ) |>
    add_trace(
      type="scatter", mode="markers",
      x=~date,
      y=~`景氣對策信號(分)`,
      marker=list(
        size=18, # point size
        # fill
        color=~I(startColor),
        gradient=list(
          type=~I(type),
          color=~I(endColor)
        ),
        # stroke
        line=list(
          color=~I(strokeColor),
          width=~I(strokeWidth)
        )
      )
    ) -> pltBase

  pltBase |>
    plotly::layout(
      title=list(
        text="景氣信號分數與信號顏色",
        pad=list(
          t="20"
        ),
        yanchor="top"
      ),
      showlegend=FALSE,
      xaxis=list(
        showgrid=FALSE,
        title=list(
          text=NULL
        )
      ),
      yaxis=list(
        tickmode="array",
        tickvals=list(
          9, 17, 23, 32, 38, 45
        ),
        title=list(
          text=NULL
        )
      )
    )
}
save_plt_businessCycle <- function() {
  if(!dir.exists("data")) dir.create("data")
  plt_businessCycle() |>
    saveRDS("data/plt_businessCycleLight.Rds")
  message("data/plt_businessCycleLight.Rds is saved.")
}
load_plt_businessCycle <- function(){
  .GlobalEnv$plt_businessCycle = readRDS("data/plt_businessCycleLight.Rds")
  message("Object plt_businessCycle is loaded to the global environment.")
}
