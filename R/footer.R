#' Footer
#'
#' @param onType one of c("首頁","找人", "找事/物","經濟數據")
#'
#' @return
#' @export
#'
#' @examples footer(onType="首頁") |> econWeb::browseTag2()
footer <- function(onType=c("首頁","找人", "找事/物","經濟數據")){
  onType = match.arg(onType)
  tagList(footer_tag(onType), footer_dependency())
}
footer_tag <-function(onType="經濟數據"){
  # onType="經濟數據"
    types = c("首頁","找人", "找事/物","經濟數據")
    footerBtns() -> btns
    whichIsOn = which(types == onType)
    btns[[whichIsOn]] <-
      btnHolder(onType, 'on')
    tags$div(
      class = "footer",
      btns)
}
btnHolder <- function(type, state){
  tags$div(class = "footer-buttonHolder",
    roundMaskBtn(type,state),
    tags$div(class = "footer-buttonHolder-Text",type))
}
footerBtns <- function(){
  tagList(
    btnHolder("首頁", "off"),
    btnHolder("找人", "off"),
    btnHolder("找事/物", "off"),
    btnHolder("經濟數據", "off")
  ) -> btnHolders
  btnHolders
}
footer_dependency <- function(){
  htmltools::htmlDependency(
    name="footer",
    version="1.0.0",
    src=c(file=normalizePath("./assets/css")),
    style="footer.css",
    all_files = F
  )}



