#' Create Plotly reference tools
#'
#' @return
#' @export
#'
#' @examples none
PlotlyTools <- function(){
  pt <- new.env()
  pt$query_trace <- {
    data("trace_urls")
    data("tracenames")
    query_tracenames = paste0("query_", tracenames)
    purrr::map(
      seq_along(trace_urls),
      ~{
        function() {
          generate_find_attribute(
            trace_urls[[.x]])-> qfun
          qfun ->
            pt[[
              query_tracenames[[.x]]
            ]]
          invisible(qfun)
        }}
    ) |>
      setNames(tracenames)
  }
  # debug(pt$query_bar)
  # pt$query_bar("color")
  # pt$query_trace$bar()("font")
  pt$query_layout <-{
    data("layout_urls")
    data("layoutreferenceNames")
    query_layoutnames = paste0("query_", layoutreferenceNames)
    purrr::map(
      seq_along(layout_urls),
      # generate_find_attribute,
      # seq_along(trace_urls),
      ~{
        function() {
          generate_find_attribute(
            layout_urls[[.x]])-> qfun
          qfun ->
            pt[[
              query_layoutnames[[.x]]
            ]]
          invisible(qfun)
        }}
    ) |>
      setNames(layoutreferenceNames)
  }

  return(pt)
}
#' Generate find_attribute function under given plotly reference url
#'
#' @param url a plotly reference url, such as https://plotly.com/r/reference/scatter/
#'
#' @return a webpage with all the result listing found.
#' @export
#'
#' @examples find_attr <- generate_find_attribute("https://plotly.com/r/reference/scatter/")
generate_find_attribute <- function(url){
  url |>
    stringr::str_remove("/$")
  # url = "https://plotly.com/r/reference/scatter/"
  # attr = "colors"
  rvest::read_html(url) -> html
  html |>
    rvest::html_elements("li") -> all_li
  all_li |>
    modify_href(url)
  function(attrname=NULL){
    all_li |>
      get_attribute_tagList(attrname) |>
      browsable()
    # all_li |> find_li_with_attribute(attrname) -> target_li
    #
    # require(htmltools)
    # tags$ul(
    #   HTML(
    #     as.character(target_li)
    #   )
    # ) |> showTag()
  }
}
modify_href <- function(target_li, url) {
  for(.li in target_li){
    .li |>
      xml2::xml_find_all("a") |>
      purrr::walk(
        ~{
          file.path(
            url, xml2::xml_attr(.x, "href") ) ->
            xml2::xml_attr(.x, "href")
        }
      )
  }
}
get_plotly_references <- function(){
  "https://plotly.com/r/reference/index/" |>
    xml2::read_html() -> html
  html |>
    rvest::html_elements(
      ".--sidebar-item a"
    ) -> targets_a
  targets_a |>
    purrr::map(
      ~xml2::xml_attr(.x, "href")
    ) -> allReferences
  targets_a |>
    rvest::html_text() -> allSubjects
  return(allReferences)
}

get_referenceNames_referenceUrls <- function(){
  allReferences |>
    stringr::str_extract("(?<=(/r/reference/)).*") -> basereferenceNames

  whichIsTrace <- which(
    !(basereferenceNames |>
        stringr::str_detect("^layout"))
  )
  file.path(
    "https://plotly.com", allReferences[whichIsTrace]
  ) -> trace_urls
  file.path(
    "https://plotly.com",
    allReferences[-whichIsTrace]
  ) -> layout_urls
  tracenames <- allSubjects[whichIsTrace]
  layoutreferenceNames <-
    allSubjects[-whichIsTrace]
  # basereferenceNames[-whichIsTrace] |>
  #   stringr::str_extract(
  #     "(?<=(layout/))[^/]*"
  #   ) -> layoutreferenceNames

}
get_html <- function(a_from_LiX) {
  a_from_LiX |>
    xml2::xml_siblings() -> a_siblings
  tagList(
    a_from_LiX |> turn_html(brFront=T) |> unique(),
    a_siblings |> turn_html() |> unique())
}


turn_html <- function(x, brFront=F){
  if(brFront){
    brOpen="<br/>"
    brClose=NULL
  } else {
    brOpen=NULL
    brClose="<br/>"
  }
  purrr::map(
    x, ~{HTML(
      c(brOpen, as.character(x), brClose))}
  )
}
get_attribute_tagList <- function(all_li, attrname=NULL) {
  purrr::map(
    all_li,
    ~{
      .x |>
        rvest::html_elements("a.attribute-name")}) ->
    list_a
  purrr::map(
    list_a,
    ~{
      .x |>
        rvest::html_text() }) -> list_attributenames
  if(is.null(attrname)) return(
    {
      list_attributenames |>
        unlist() |>
        stringr::str_remove_all('\\s')

      })
  purrr::map(
    list_attributenames,
    ~{
      which(stringr::str_detect(.x, glue::glue("\\b{attrname}\\b")))
    }
  ) -> amongWhichAIsTarget

  purrr::map_lgl(
    amongWhichAIsTarget,
    ~{length(.x)!=0}
  ) -> pickLi
  amongWhichAIsTarget2 <- amongWhichAIsTarget[pickLi]
  all_li[pickLi] -> LiPicked
  APicked <- list_a[pickLi]
  # .x=25
  list_html <- vector("list", length(APicked))
  for(.x in seq_along(APicked)){
    a_from_LiX <- APicked[[.x]][amongWhichAIsTarget2[[.x]]]
    a_from_LiX |>
      purrr::map(get_html) -> list_html[[.x]]
  }
  unique(list_html) -> list_html
  list_html |>
    as.tags()
}





# find_trace_attribute$bar("family")
# find_trace_attribute$scatter("color")
# find_layout_attribute$xaxis("automargin")
