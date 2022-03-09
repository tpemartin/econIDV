get_layout_qfun <- function(targetUrl, layout_section_urls, isLayoutSection) {
  targetUrl |>
    stringr::str_detect("#") -> flag_section
  if(flag_section){
    generate_find_attribute_selectedAllLi(
      targetUrl, layout_section_urls
    ) -> qfun
  } else {
    generate_find_attribute(
      targetUrl, isLayoutSection)-> qfun
  }
  qfun
}



generate_find_attribute_selectedAllLi <- function(url, layout_section_urls) {
  url |>
    get_valid_all_li(layout_section_urls) -> all_li
  urlNew=stringr::str_remove(url, "/#.*$")
  all_li |>
    modify_href(urlNew)
  function(attrname=NULL, regex=F, onlyTitle=T, checkHref=T){
    if(is.null(attrname)){
      attrname=".*"
      regex <- onlyTitle <- checkHref <- T
    }
    all_li |>
      get_attribute_tagList(attrname, regex, onlyTitle, checkHref) -> resultTagList
    resultTagList |>
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
get_valid_all_li <- function(targetUrl, layout_section_urls) {
  targetUrl |>
    rvest::read_html() -> html
  html |>
    rvest::html_elements("li") -> all_li
  all_li |> get_list_href() -> list_hrefs
  list_hrefs |>
    get_layoutSectionStart(layout_section_urls) -> sectionStarts
  sectionEnds <- c(sectionStarts[2:length(sectionStarts)]-1, length(all_li))
  whichSectionStarts = which(layout_section_urls==targetUrl)
  valid_all_li = all_li[
    sectionStarts[[whichSectionStarts]]:sectionEnds[[whichSectionStarts]]
  ]
  valid_all_li
}
get_layoutSectionStartX <- function(all_li, layout_section_urlsX) {
  all_li |> get_list_href() -> list_hrefs
  layout_section_urlsX |> stringr::str_extract("#.*$") -> targetSection
  pick_list_hrefs <- vector('logical', length(list_hrefs))
  for(.x in seq_along(list_hrefs)){
    if(length(list_hrefs[[.x]])==0 ||
        is.na(list_hrefs[[.x]][[1]])) next
    hrefX =list_hrefs[[.x]][[1]]
    pick_list_hrefs[[.x]] <- hrefX == targetSection
  }
  whichIsSectionLiStart = max(which(pick_list_hrefs))
  whichIsSectionLiStart
}

get_valid_first_hrefs <- function(list_hrefs) {
  valid_first_hrefs <- vector('character', length(list_hrefs))
  for(.x in seq_along(list_hrefs)){
    if(length(list_hrefs[[.x]])==0 ||
        is.na(list_hrefs[[.x]][[1]])) next
    hrefX =list_hrefs[[.x]][[1]]
    valid_first_hrefs[[.x]] <- hrefX
  }
  valid_first_hrefs
}

get_layoutSectionStart <- function(list_hrefs, layout_section_urls) {
  valid_first_hrefs <- get_valid_first_hrefs(list_hrefs)
  purrr::map_int(
    seq_along(layout_section_urls),
    ~{
      # .x=1
      layout_section_urls[[.x]] |>
        stringr::str_extract("#.*$") -> targetSection
      which(valid_first_hrefs == targetSection ) |>
        max()
    }
  )
}
