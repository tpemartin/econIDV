#' Turn off certain graph elements in ggplot2
#'
#' @return a turnOff instance
#' @export
#'
ThemeTurnOff <- function(){
  turnOff <- new.env()
  turnOff$theme <- list()
  turnOff$axis.line = function(){
    turnOff$theme <-
      append(
        turnOff$theme,
        list(theme(axis.line=element_blank())))}
  turnOff$axis.title = function(){
    turnOff$theme <-
      append(
        turnOff$theme,
        list(theme(axis.title=element_blank())))}
  turnOff$axis.ticks = function(){
    turnOff$theme <-
      append(
        turnOff$theme,
        list(theme(axis.ticks=element_blank())))}
  turnOff$axis.line.x= construct_turnOff(turnOff,"axis.line.x")
  turnOff$axis.line.y= construct_turnOff(turnOff,"axis.line.y")
  turnOff$axis.title.x= construct_turnOff(turnOff,"axis.title.x")
  turnOff$axis.title.y= construct_turnOff(turnOff,"axis.title.y")
  turnOff
}
construct_turnOff = function(turnOff, element){
  function(){
    # element="axis.line.x"
    listArg=list()
    listArg[[element]] <- ggplot2::element_blank()
    themeXoff = do.call(ggplot2::theme,listArg)
    turnOff$theme <-
      append(
        turnOff$theme,
        list(themeXoff))}
}
#' Generate scale_x/y that removes margins on the axes.
#'
#' @param ... scale_x, scale_y functions
#'
#' @return a list of scale function using which will remove the axes.
#' @export
ScaleOff = function(...)
{
  exprsArg = rlang::enexprs(...)
  listArg=list(...)
  scaleOff = list()
  for(.x in seq_along(exprsArg)){
    scaleXexpr = exprsArg[[.x]]
    rlang::expr(
      function(...){
        (!!scaleXexpr)(expand=expansion(0,0), ...)
      }
    ) -> exprScaleX
    scaleOff[[deparse(scaleXexpr)]] <-
      eval(exprScaleX)
  }
  scaleOff
}
