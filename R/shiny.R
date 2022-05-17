#' Get all input/output ids from UI code in the clipboard and attach input output lists for typing double check
#'
#' @return attach an environment of input/output
#' @export
attach_io_fromClipboardUI <- function(){
  get_inputId_fromClipboardUI() -> list_ioIds
  attach_input_output(list_ioIds)
}

get_inputId_fromClipboardUI <- function() {
  rlang::parse_expr(
    paste0(clipr::read_clip(), collapse ="")
  ) -> uiExpr
  lobstr::ast(!!uiExpr) -> toDo

  inputIds=character(0)
  which(stringr::str_detect(toDo, "\"", negate=T) & stringr::str_detect(toDo, "Input")) -> whichAreInputFunctions
  if(length(whichAreInputFunctions)!=0) inputIds = toDo[whichAreInputFunctions+1] |>
    stringr::str_extract('(?<=").*(?=")')

  outputIds=character(0)
  which(stringr::str_detect(toDo, "\"", negate=T) & stringr::str_detect(toDo, "Output")) -> whichAreOutputFunctions
  if(length(whichAreOutputFunctions)!=0) outputIds = toDo[whichAreOutputFunctions+1] |>
    stringr::str_extract('(?<=").*(?=")')

  list(inputIds=inputIds, outputIds=outputIds)
}

attach_input_output <- function(list_ioIds) {
  inputIds=list_ioIds$inputIds
  outputIds=list_ioIds$outputIds

  io = new.env()
  if(length(inputIds)!=0){
    io$input = vector("list", length(inputIds))
    names(io$input) = inputIds
  }
  if(length(outputIds)!=0){
    io$output = vector("list", length(outputIds))
    names(io$output) = outputIds
  }

  attach(io)
}
