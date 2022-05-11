#' Supply titles to the subplots panel produced by plotly::subplot()
#'
#' @param id default=NULL
#' @param widget a widget
#' @param titles list or character vector of titles
#' @param lower default="40px", the position titles should be lowered by this amount
#' @param fontsize default="20px"
#' @param pull default=0, the left and right pull add padding to both sides
#' @param leftpull default=0, the left padding
#' @param rightpull default=0, the right padding
#'
#' @return
#' @export
#'
#' @examples none.
subplot_titles <- function(widget, titles, id=NULL,  lower="40px",fontsize="20px", pull=0,
  leftpull=0, rightpull=0) {

  lower=paste0("-", lower)
  crosstalk::bscols(
    widths=c(12, 12),
    ui_container_labels(
      id,
      label=titles, lower, fontsize, pull,
      leftpull, rightpull),
    widget
  )
}

fig_update_containerLOT <- function() {
  fig = econWeb::Fig()
  fig$export("inst/html/css/container_labels")
}


tag_container_labels <-function(id, label, lower, fontsize, leftpull, rightpull){
  .css=css(bottom=lower, `font-size`=fontsize)
  tags$div(id=id, class = "toplabelContainer-label",
    toplabels(label, lower, fontsize, leftpull, rightpull)

  )
}
toplabel = function(text){
  tags$div(class = "toplabelContainer-label-text",
    # style=.css,
    text)
}
toplabels = function(label, lower, fontsize, leftpull, rightpull){

  #
  # nlast = length(label)
  purrr::map(
    seq_along(label),
    ~{
      # if(.x==1){ .css=css(bottom=lower, `font-size`=fontsize, `padding-left`=leftpull)
      # } else if (.x==nlast){
      #   .css=css(bottom=lower, `font-size`=fontsize, `padding-right`=rightpull)
      # } else {
      #   .css=css(bottom=lower, `font-size`=fontsize)
      # }
      toplabel(label[[.x]]) #, .css)
    }
    )
}
pull_dependency <- function(id, leftpull, rightpull){
  leftpull=paste0(leftpull, "vw")
  rightpull=paste0(rightpull, "vw")
  glue::glue("<style>
  #<<id>> > div:first-child {
    padding-left: <<leftpull>>;
  }
  #<<id>> > div:last-child {
    padding-right: <<rightpull>>;
  }
    </style>", .open="<<", .close=">>") -> .head

  htmltools::htmlDependency(
    name=id,
    version="1.0.0",
    src="",
    head=.head
  )
}
container_labels_dependency <- function(){

  htmltools::htmlDependency(
    name="econIDV",
    version="1.0.0",
    src=c(file=system.file("/html/css",  package = "econIDV")),
    style="container_labels.css",
    all_files = F
  )}

ui_container_labels <- function(id=NULL, label, lower, fontsize, pull,
  leftpull, rightpull, dependency=NULL){

  if(is.null(id)) {
    sample(letters, 5, T) -> id
    id = paste0(id, collapse="")
  }
  if(pull!=0) leftpull=rightpull=pull
  tagList(tag_container_labels(id, label, lower, fontsize, leftpull, rightpull), container_labels_dependency(),
    pull_dependency(id, leftpull, rightpull), dependency)
}
# ui_container_labels() |> econWeb::browseTag2()

