#' Show the JSON as a list from a htmlwidget
#'
#' @param tag a htmlwidget that has gone through showTag()
#'
#' @return
#' @export
#'
#' @examples
#' tag |> showTag();
#' tag |> showJson();
showJson <- function(tag){
  quo_tag = rlang::enquo(tag)
  tagname = rlang::as_label(quo_tag)
  file.path(
    "temp",
    paste0(tagname, ".html")
  ) -> filename
  filename |>
    xml2::read_html() -> xhtml
  xhtml |>
    xml2::xml_find_all(
      "//script[@type=\"application/json\"]"
    ) |>
    purrr::map(
      ~{ xml2::xml_text(.x) |>
          jsonlite::fromJSON()}
    ) -> list_x
  return(list_x[[1]])
}
