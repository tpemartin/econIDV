#' List the difference of traces from two plots
#'
#' @param plt0 current plotly plot
#' @param plt1 new plotly plot
#' @param traceId the name of one plotly_build data column that identifies trace. If not sure, checkTraceColumns(plt0) will give you all the available columns across traces.
#'
#' @return a list of two, one tell which traces have changed, the other give the restyle content of each changed trace.
#' @export
get_restyleInfo <- function(plt0, plt1, traceId) {
  list(plt0, plt1) |>
    purrr::map(
      ~{
        # .x=plt0
        px=plotly::plotly_build(.x)
        px$x$data
      }) ->
    list_data

  list_data[[1]] |> viewTraceInfo(viewer=F) -> traceInfo1
  list_data[[2]] |> viewTraceInfo(viewer=F) -> traceInfo2

  seq_along(traceInfo2[[traceId]]) |>
    setNames(
      unlist(traceInfo2[[traceId]])
    ) -> mapName2trace
  traceInfo1[[traceId]] |> unlist() -> fromName
  alignedData2=list_data[[2]][mapName2trace[fromName]]
  purrr::map2(
    list_data[[1]], alignedData2,
    ~{
      # .x=list_data[[1]][[1]]
      # .y=alignedData2[[1]]
      get_different_attrs(.x, .y) -> differentStyles
      differentStyles[!is.na(differentStyles)]
    }
  ) -> differencesByTrace
  purrr::map_lgl(
    differencesByTrace,
    ~(length(.x)!=0)
  ) |>
    which() -> whichTraces2UpdateStyle
  list(
    trace=whichTraces2UpdateStyle,
    style=differencesByTrace[whichTraces2UpdateStyle]
  )
}
#' check trace columns from a given plotly plot
#'
#' @param plt a plotly plot
#'
#' @return a data frame of trace feature columns
#' @export
checkTraceColumns <- function(plt){
  plt |>
    plotly::plotly_build() -> p
  p$x$data |> purrr::transpose() |>
    list2DF()
}
get_different_attrs = function(dataX, dataY){
  purrr::map(
    seq_along(dataX),
    ~{
      if(identical(dataX[[.x]], dataY[[.x]])) return(NA)
      dataY[[.x]]
    }
  ) |>
    setNames(names(dataX))
}

viewTraceInfo <- function(list_dataX, viewer=T){

  list_dataX |> purrr::transpose() |>
    list2DF() -> df
  if(viewer) View(df)
  invisible(df)
}
