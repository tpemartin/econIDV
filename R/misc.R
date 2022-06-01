#' Convert RBGA or RBG to hex
#'
#' @param ... A character of "rgba(168, 207, 246, 0.05941)", or inputs of rgba numbers, e.g. 168, 207, 246, 0.05941
#'
#' @return a hex code
#' @export
#'
#' @examples
#' "rgba(168,207,245,0.6941)" |> rgba2hex()
#' rgba2hex(168, 207, 245, 0.6)
rgba2hex <- function(...){
  arglist =list(...)
  if(length(arglist)==1 && is.character(arglist[[1]])){
    "rgba(168,207,245,0.6941)" |>
      stringr::str_extract_all("[0-9.]+") |>
      unlist() |>
      as.numeric() -> rgba
  } else {
    as.numeric(arglist) -> rgba
  }
  if(any(rgba>1)){
    rgba[1:3] <- rgba[1:3]/255
  }
  rgba|> as.list()
  do.call(colorspace::RGB, as.list(rgba[1:3])) -> rgb
  rgb |> colorspace::hex() -> hex
  if(length(rgba)==4){
    scales::alpha(hex, alpha=rgba[[4]]) -> hex
  }
  return(hex)
}



#' scales::show_col's revision where labels can be text other than color codes.
#'
#' @param colours characters of color
#' @param labels characters of labels
#'
#' @return
#' @export
#'
#' @examples none.
show_col2 = function(colours, labels, label_size=NULL){
  require(rlang)
  n <- length(colours)
  ncol <- ceiling(sqrt(length(colours)))
  nrow <- ceiling(n/ncol)
  colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
  colours <- matrix(colours, ncol = ncol, byrow = TRUE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colours))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
    axes = FALSE)
  rect(col(colours) - 1, -row(colours) + 1, col(colours),
    -row(colours), col = colours, border = "white")
  stringr::str_length(labels) |> max() -> maxLen
  if(is.null(label_size)){
    cex_label = min(1, 1/(maxLen/5))
  } else {
    cex_label=label_size
  }
  labels = paste0(seq_along(labels),"\n", labels)
  if(length(labels) < nrow*ncol) {
    nEmpty = nrow*ncol-length(labels)
    labels=c(labels, rep("", nEmpty))
  }
  matrix(labels, nrow=nrow, ncol=ncol, byrow=T)-> matLabels
  hcl <- farver::decode_colour(colours, "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  text(col(colours) - 0.5, -row(colours) + 0.5, matLabels,
    cex = cex_label, col = label_col)
}

#' From a character of colname, generate its I(objectName) expression in quo
#'
#' @param colname a character of variable name, such as "y2012".
#'
#' @return its quo(I(y2012))
#' @export
#'
#' @examples none.
quo_as.is <- function(colname){
  vname=as.name("y2012")
  quo_asIs=rlang::quo(I(!!vname))
  quo_asIs
}
remove_unwanted <- function(){
  allObjects <- ls(envir=.GlobalEnv)
  purrr::map_lgl(
    allObjects,
    ~{class(get(.x, envir=.GlobalEnv))-> possibleClasses
      any(possibleClasses %in% c("data", "environment", "function")) -> flag_classOkay
      flag_classOkay
    }
  ) -> flag_classOkay
  flag_exceptions = allObjects %in% c(.GlobalEnv$.exceptions, ".exceptions")
  flag_toKeep = flag_classOkay || flag_exceptions

  rm(list=allObjects[!flag_toKeep], envir=.GlobalEnv)
}

#' Add data to object's class
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples none
as.Data <- function(object) {
  base::class(object) <- c(base::class(object), "data") |> unique()
  return(object)
}
#' Show X
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples none
showX <- function(obj){
  obj$x |> listviewer::jsonedit()
}
#' Get input arguments' names and their defaults
#'
#' @param fn a function
#'
#' @return
#' @export
#'
#' @examples none.
get_inputNamesDefaults <- function(fn) {
  fn |> formals() |> names() -> keys
  fn |> formals() |> as.character() -> values
  data.frame(argument=keys, default=values)
}

unload_reload <- function(){
  unloadNamespace("econIDV")
  library(econIDV)
}
#' As browseURL but it browse a shiny.tag or html class object in RStudio Viewer
#'
#' @param tag A shiny.tag or html class object
#'
#' @return
#' @export
#'
#' @examples none
showWidget <- function(.tag=.Last.value){
  if(!dir.exists("temp")) dir.create("temp")
  servr::daemon_stop()
  htmltools::save_html(
    htmltools::tagList(.tag, dep_mobile()), file=file.path("temp","temp.html")
  )
  ss <- servr::httd("temp")
  # ss$port
  rstudioapi::viewer(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
}
dep_mobile <- function(){
  htmltools::htmlDependency(
    name="temp",
    version="1.0.0",
    src=c(file="assets/"),
    meta=list(
      viewport="width=device-width, initial-scale=1.0"
    )
  ) -> dep_mobile
}
getDupnames <- function(argList) {
  argList |>
    purrr::map(names) -> listnames
  unlist(listnames) -> allnames
  table(allnames) -> tbnames

  duplicateNames = names(tbnames)[tbnames!=1]
  okaynames = c(names(tbnames)[tbnames==1],"")
  list(
    duplicateNames=duplicateNames,
    okaynames=okaynames)
}
get1stLevelLists2merge <- function(argList) {
  listNames= setdiff(getDupnames(argList),"")
  duplicateNames=listNames$duplicateNames
  okNames = listNames$okaynames
  argList |>
    purrr::map(names) -> listnames
  purrr::map(
    seq_along(argList),
    ~{pickOK = (names(argList[[.x]]) %in% okNames)
    if(sum(pickOK)==0){
      list()
    } else {
      argList[[.x]][pickOK]
    }
    }) -> listOK
  unlist(listOK, recursive=F) -> listOK

  dupList <- vector("list", length(duplicateNames))
  if(length(duplicateNames)==0){
    return(list())
  } else {
    for(.x in seq_along(duplicateNames)){
      dupnameX = duplicateNames[[.x]]
      listnames |> purrr::map_lgl(
        function(x) purrr::has_element(x, dupnameX)
      ) -> pickListWithDupnamex
      argList[pickListWithDupnamex] |>
        purrr::map(
          function(x) purrr::pluck(x, dupnameX)
        ) -> list2merge
      dupList[[.x]]=list2merge
    }
    names(dupList) = duplicateNames

  }

  c(dupList, listOK)
}
#' Merge two list while respecting original list structure
#'
#' @param ... list arguments
#'
#' @return a merged list
#' @export
#'
merge_list <- function(...){
  argList = list(...)
  purrr::map_lgl(
    argList,
    ~length(.x)!=0
  ) -> pick_noEmpty

  append(list(list()), argList[pick_noEmpty]) -> seqList
  seqList |>
    purrr::reduce(recreate_list)
}
recreate_list <- function(result=list(), .list) {
  unlist(.list) -> unlistArgs
  # list(a,b) |> unlist() -> unlistArgs
  # unlist(a) -> unlistArgs
  # unlistArgs
  unlistArgs |> names() |>
    sort() |>
    stringr::str_extract_all("[^.]+","") -> listNames
  listRetrieval <- vector("list", length(listNames[,1]))
  for(.x in seq_along(listNames[,1])){
    #.x=2
    typeof(unlistArgs[[1]])

    listNames[.x,] |> na.omit() -> listNamesX
    paste0(paste0("[['",listNamesX, "']]"), collapse = "") -> retrievalX
    typeof(unlistArgs[[.x]]) -> argType
    paste0("result", retrievalX,("<- .list"), retrievalX) -> listRetrieval[[.x]]

    #unlistArgs[[.x]]") ->listRetrieval[[.x]]
  }
  # result=list()
  callenv=rlang::current_env()
  purrr::walk(
    seq_along(listRetrieval),
    ~eval(parse(text=listRetrieval[[.x]]), envir = callenv)
  )
  result
}
