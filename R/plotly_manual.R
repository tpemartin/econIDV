#' Create Plotly reference tools
#'
#' @return
#' @export
#'
#' @examples none
PlotlyTools <- function(){
  pt <- new.env()
  pt$query_trace <- {
    trace_urls = econIDV::trace_urls
    tracenames = econIDV::tracenames
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
    layout_urls = econIDV::layout_urls
    layoutreferenceNames = econIDV::layoutreferenceNames

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
  # url = "https://plotly.com/r/reference/scatter/"
  url |>
    stringr::str_remove("/$")
  # url = "https://plotly.com/r/reference/scatter/"
  # attr = "colors"
  rvest::read_html(url) -> html
  html |>
    rvest::html_elements("li") -> all_li
  all_li |>
    modify_href(url)
  function(attrname=NULL, regex=F){
    all_li |>
      get_attribute_tagList(attrname, regex) |>
      htmltools::browsable()
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
get_html <- function(a_from_LiX, hrefX) {
  a_from_LiX |>
    xml2::xml_siblings() -> a_siblings
  htmltools::tagList(
    htmltools::tags$hr(),
    htmltools::tags$h4(hrefX),
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
    x, ~{htmltools::HTML(
      c(brOpen, as.character(x), brClose))}
  )
}
get_attribute_tagList <- function(all_li, attrname=NULL, regex=F) {
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

  which_fix = ifelse(regex, which_fitRegex, which_fitWord)
  purrr::map(
    list_attributenames,
    ~{
      which_fix(.x, attrname)
    }
  ) -> amongWhichAIsTarget

  purrr::map_lgl(
    amongWhichAIsTarget,
    ~{length(.x)!=0}
  ) -> pickLi
  amongWhichAIsTarget2 <- amongWhichAIsTarget[pickLi]
  all_li[pickLi] -> LiPicked
  LiPicked |> get_list_href() -> list_hrefs
  APicked <- list_a[pickLi]
  # .x=25
  list_html <- vector("list", length(APicked))
  for(.x in seq_along(APicked)){
    a_from_LiX <- APicked[[.x]][amongWhichAIsTarget2[[.x]]]
    hrefX <- list_hrefs[[.x]][amongWhichAIsTarget2[[.x]]]
    a_from_LiX |>
      purrr::map2(hrefX, get_html) -> list_html[[.x]]
  }
  unique(list_html) -> list_html
  list_html |>
    htmltools::as.tags()
}

# helpers -----------------------------------------------------------------
get_attributes_structure <- function(LiPicked) {
  LiPicked |>
    get_href_all() -> list_hrefs
  attStructure <- new.env()
  list_hrefs |>
    purrr::map(
      ~{ stringr::str_extract(.x, "#.*$") }
    ) -> list_hrefs
  attStructure$hrefStructure <- list()
  for(.y in seq_along(list_hrefs)){
    # .x=190
    list_hrefs[[.y]] |>
      stringr::str_replace_all("[#-]", "$") -> retrieveString
    purrr::walk(
      retrieveString,
      ~evalRetrieveString_safely(.x, .y, attStructure)
    )

  }
  # attStructure$hrefStructure |> names() |>
  #   stringr::str_which("", negate=F) -> tracename
  # attStructure$hrefStructure[[tracename]]
  attStructure$hrefStructure
}

which_fitWord <- function(.x, attrname) {
  which(
    stringr::str_detect(.x, glue::glue("\\b{attrname}\\b"))
  )
}
which_fitRegex <- function(.x, pattern) {
  which(
    stringr::str_detect(.x, pattern)
  )
}

get_href <- function(el){
  rvest::html_elements(el, "a") -> nodeSet
  rvest::html_attr(
    nodeSet,
    "href")
}
get_href_safely = purrr::safely(get_href)

get_href_all <- function(all_li) {
  all_li |>
    purrr::map(
      ~get_href_safely(.x)
    ) -> list_href

  list_href |>
    purrr::map(
      purrr::pluck("result")
    ) -> list_href
  return(list_href)
}

evalRetrieveString <- function(.x, .y, attStructure){
  # .x=retrieveString[[1]]
  # .y=1
  rlang::parse_expr(
    paste0("hrefStructure",.x,"<-",.y)) |>
    rlang::eval_bare(env = attStructure)
}
evalRetrieveString_safely = purrr::safely(evalRetrieveString)
get_list_href <- function(LiPicked) {
  LiPicked |>
    get_href_all() -> list_hrefs

  list_hrefs |>
    purrr::map(
      ~{ stringr::str_extract(.x, "#.*$") }
    ) -> list_hrefs
}



# find_trace_attribute$bar("family")
# find_trace_attribute$scatter("color")
# find_layout_attribute$xaxis("automargin")
