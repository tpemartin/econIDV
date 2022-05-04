#' Convert 6 digit hex color into a statement of "rgba(r, g, b, a)"
#'
#' @param hexcolor a character vector of 6 digits hex
#' @param alpha numeric, between 0 and 1. Default=1
#'
#' @return
#' @export
#'
#' @examples hex2rgba("#294968", "#217BBB")
hex2rgba <- function(hexcolor, alpha=1) {
  hexcolor |>
    colorspace::hex2RGB() -> rgbcolor
  rgbcolor@coords*255 |> round(digits = 0) -> rgb255color
  purrr::map_chr(1:nrow(rgb255color),
    ~{
      rgb255colorX = rgb255color[.x, ]
      glue::glue(
        'rgba({rgb255colorX[["R"]]}, {rgb255colorX[["G"]]},{rgb255colorX[["B"]]}, {alpha})')
    })
}


script_plotly <- function(plt0, id) {
  plt0 |>
    htmlwidgets:::createPayload() -> payload
  payload$x |> htmlwidgets:::toJSON() -> jsonX
  plotly_script(id, jsonX)
}

plotly_script <- function(id, jsonX){
  tags$script(
    glue::glue(
      "TESTER = document.getElementById('{id}');
  	Plotly.newPlot( TESTER, {jsonX} );"
    )) |>
    tagList(
      Dependencies()$plotly()
    )
}

find_li_with_attribute <- function(all_li, attr){
  purrr::map(
    all_li,
    ~{
      .x |>
        rvest::html_elements("a.attribute-name") |>
        rvest::html_text() }) -> list_attributenames
  purrr::map_lgl(
    list_attributenames,
    ~{
      any(stringr::str_detect(.x, glue::glue("\\b{attr}\\b")))
    }
  ) |> which() -> whichHasTargetAttribute
  all_li[whichHasTargetAttribute]
}
