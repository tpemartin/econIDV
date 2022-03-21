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
