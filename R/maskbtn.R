tag_roundMaskBtn <-function(imgUrl, state){
  color = c("on"="#6fcf97", "off"="#000000")
  # .css=css("border-color"=color[[state]])
  .css=css(
    "border-color"="#000000",
    "box-shadow"="0px 4px 4px rgba(0, 0, 0, 0.25);"
    )
  if(state=="off") {
    # .css=paste0(
    #   .css, css("box-shadow"="0px 4px 4px rgba(0, 0, 0, 0.25);"))
    .class="btnRound"} else
    {
      .class="btnRound btn-on"
    }
  tags$div(class = .class,
    # style=.css,
    tags$div(class = "btnRound-maskGraph",
      tags$div(class = "btnRound-maskGraph-color",
        style=css(
          mask = glue::glue("no-repeat center url({imgUrl})"),
          `-webkit-mask`= glue::glue("no-repeat center url({imgUrl})")
        ))))
}
roundMaskBtn_dependency <- function(){
  htmltools::htmlDependency(
    name="maskbtn",
    version="1.0.0",
    src=c(file=normalizePath("./assets/css")),
    style="maskbtn.css",
    all_files = F
  )}
roundMaskBtn <- function(imgUrl="/lib/attachment-1/graph.svg", color="#000000"){
  tagList(tag_roundMaskBtn(imgUrl, color), roundMaskBtn_dependency())
}

#' Rounded masked button
#'
#' @param type one of c("首頁","找人", "找事/物","經濟數據")
#' @param state one of c("on", "off")
#'
#' @return
#' @export
#'
#' @examples roundMaskBtn(type="首頁", state="off") |> econWeb::browseTag2()
roundMaskBtn <- function(type=c("首頁","找人", "找事/物","經濟數據"), state=c("on","off")){
  type=match.arg(type)
  state=match.arg(state)

  switch(
    type,
    "首頁"={"./lib/attachment-1/home.svg"},
    "找人"={"./lib/attachment-1/address.svg"},
    "找事/物"={"./lib/attachment-1/news.svg"},
    "經濟數據"={"./lib/attachment-1/graph.svg"}
  ) -> imgUrl

  # browser()
  tagList(tag_roundMaskBtn(imgUrl, state), roundMaskBtn_dependency())
}

