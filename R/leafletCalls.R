#' Create leaflet tools to translate R calls to JS expressions
#'
#' @description Create a tools environment.
#' @return a tool environment
#' @export
LeafletTools <- function() {
  lf <- new.env()
  lf$getTileArgsFromCall <- getTileArgs(lf)
  lf$getMarkerArgsFromCall <- getMarkerArgs
  return(lf)
}
getTileArgs <- function(lf) {
  function(callx) {
    callx$args |>
      purrr::keep(~ {
        length(.x) != 0
      }) -> xx
    lf$tile$args <- xx
    lf$tile$js <- function() {
      createJStile(lf$tile$args)
    }
  }
}
getMarkerArgs <- function(lf) {
  function(callx) {
    args <- callx$args
    list(
      lat = args[[1]],
      lng = args[[2]],
      markerOptions = args[[6]] |> jsonlite::toJSON(auto_unbox = T),
      popup = ifelse(is.null(args[[7]]),
        NULL,
        list(
          content = args[[7]],
          options = {
            args[[12]] |> jsonlite::toJSON(auto_unbox = T)
          }
        )
      )
    ) -> markerArgs

    lf$marker$args <- markerArgs
    lf$marker$js <- function() {
      createJSmarker(lf$marker$args)
    }
  }
}
createJStile <- function(tileArgs) {
  glue::glue("//tile layer
  t = L.tileLayer(...{tileArgs |> jsonlite::toJSON(auto_unbox = T)})") -> xx
  xx |> clipr::write_clip()
  xx
}
createJSmarker <- function(markerArgs) {
  glue::glue('// markers\nvar latLng = L.latLng({markerArgs$lat},{markerArgs$lng});\nvar mk = L.marker(latLng,\n\t{markerArgs$markerOptions})\nmk.bindPopup("{markerArgs$popup$content}",\n\t{markerArgs$popup$options})\n') -> xx
  xx |> clipr::write_clip()
}
