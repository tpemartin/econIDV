
page_ui <-function(content=sectionPanel(), footerOn=c("首頁","找人", "找事/物","經濟數據")){
  footerOn=match.arg(footerOn)
    tags$div(class = "page",
      # tags$div(
        # class = "page-sectionPanel",
      content,
        # sectionContainer()),
      footer(onType=footerOn)
      )
}
sectionHeader <- function(){tags$div(class = "sectionHeader",
  tags$div(class = "sectionHeader-title"),
  tags$div(class = "sectionHeader-logo"))}
sectionContainer <- function(){
  tags$div(class = "sectionContainer",
    sectionHeader(),
  tags$div(class = "sectionContainer-sectionGraph"),
  tags$div(class = "sectionContainer-sectionTitle"),
  tags$div(class = "sectionContainer-sectionArticle"))}
page_dependency <- function(){
  htmltools::htmlDependency(
    name="page",
    version="1.0.0",
    src=c(file=normalizePath("./assets/css")),
    style="page.css",
    all_files = F
  )}
attachment_dependency <- function(){
  htmltools::htmlDependency(
    name="attachment",
    version="1",
    src=c(file=normalizePath("/Users/martinl/Github/econIDV/assets/img")),
    attachment="",
    all_files = T
  )}
pageUI <- function(content=sectionPanel(), footerOn=c("首頁","找人", "找事/物","經濟數據")){
  tagList(page_ui(content, footerOn), page_dependency(), attachment_dependency())
}
# pageUI(footerOn="經濟數據") |> econWeb::browseTag2()
pageUI(
  content=sectionPanel(
    imgUrl="", imgWidget=plt),
  footerOn="經濟數據"
  ) |> econWeb::browseTag2()
