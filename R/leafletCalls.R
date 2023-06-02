#' Create leaflet tools to translate R calls to JS expressions
#'
#' @description Create a tools environment.
#' @return a tool environment
#' @export
LeafletTools <- function(m) {
  library(leaflet)


  lf <- new.env()

  lf$calls <- {
    # return a vector of functions. each generates the corresponding
    # js expression for the corresponding call
    m$x$call |>
      purrr::map(
        ~{
          function(){
            getJsExpressionForCall(.x)
          }
        }
      )


  }

  lf$allCalls <- function(){
    # return one js expression for all calls together
    lf$calls |>
      purrr::map(~{.x()})
  }

  return(lf)
}

getJsExpressionForCall=function(callx){

  switch(
    callx$method,
    "addMarkers"={
      callxArgs = callx$args
      ix = c(1,2,6,7,12)
      names(callxArgs)[ix] <-
        c("lat","lng","marketOptions", "popupContent", "popupOptions")

      callxArgs[-ix] <- NULL

      callxArgs |>
        jsonlite::toJSON(auto_unbox = T) -> jsArgs
      popup = ifelse(is.null(callxArgs$popupContent),"",
                     "mk.bindPopup(args.popupContent,args.popupOptions)")
      "
// {callx$method}
args={jsArgs}
latLng = L.latLng(args.lat,args.lng)
mk = L.marker(latLng, args.markerOptions)
{popup}
mk.addTo(m)
" |> glue::glue() -> jsExpression
      jsExpression
    },
    "addCircleMarkers"={

      ix=c(1,2,3,6,9,12)
      callxArgs <- callx$args
      names(callxArgs)[ix] <-
        c("lat","lng", "radius","markerOptions", "popupContent", "popupOptions")
      callxArgs[-ix] <- NULL

      list(radius=callxArgs$radius) |>
        append(callxArgs$markerOptions) -> callxArgs$markerOptions

      callxArgs |>
        jsonlite::toJSON(auto_unbox = T) -> jsArgs

      if(is.null(callxArgs$popupContent)){
        popup=""
      } else {
        popup="mk.bindPopup(args.popupContent,args.popupOptions)"
      }

      "
// {callx$method}
args={jsArgs}
latLng = L.latLng(args.lat,args.lng)
mk = L.circleMarker(latLng, args.markerOptions)
{popup}
mk.addTo(m)
" |> glue::glue() -> jsExpression
      jsExpression
    },
    "addTiles"={
      callx$args |>
        purrr::keep(~ {
          length(.x) != 0
        }) -> argsx
      "
// {callx$method}
t = L.tileLayer(...{argsx |> jsonlite::toJSON(auto_unbox = T)})
t.addTo(m)
" |>
        glue::glue() -> jsExpression
      jsExpression
    }
  )
}


