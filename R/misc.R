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
showWidget <- function(tag=.Last.value){
  if(!dir.exists("temp")) dir.create("temp")
  servr::daemon_stop()
  htmltools::save_html(
    tagList(tag, dep_mobile()), file=file.path("temp","temp.html")
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
