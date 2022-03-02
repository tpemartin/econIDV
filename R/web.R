#' Create a web instance
#'
#' @return
#' @export
#'
#' @examples none.
Web <- function(){
  require(htmltools)
  webExists = exists("web", envir=.GlobalEnv)
  if(!webExists)
  {webname = "web"
  message("will create a object named 'web' in your global environment.")
  # Sys.setenv("econWeb.webname"="web")
  }
  deps = webtheme::generate_webtheme()

  web=new.env()
  web$.tag <- list()

  web$create_page <- function(tag=NULL, destfolder=NULL, filename=NULL){
    if(is.null(destfolder)) destfolder="temp"
    if(!dir.exists(destfolder))
    {  message(
      glue::glue("Creating a folder '{destfolder}' to host the web page."))
      dir.create(destfolder)
    }
    if(is.null(filename)) filename="index.html"
    filepath=file.path(destfolder, filename)
    tagList(
      tag,
      web$.tag
    ) -> web$page

    web$page |>
      htmltools::save_html(filepath)
    message(filepath, "\nis created.")

    web$show_page <- function(){
      browseURL(
        filepath
      )
    }

  }

  web$add_jQuery <- function(){
    dep_jquery =
      htmltools::htmlDependency(
        name = "jquery-mini",
        version = "3.6.0",
        src = list(
          href = "https://code.jquery.com"),
        meta = NULL,
        script = "jquery-3.6.0.min.js",
        stylesheet = NULL, head = NULL,
        attachment = NULL, package = NULL,
        all_files = TRUE)

    append(web$.tag, list(dep_jquery)) -> web$.tag
  }
  web$add_materialise <- function(){
    dep_materialise <-
      htmltools::htmlDependency(
        name = "materialise",
        version = "1.0.0",
        src = list(
          href = "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0"),
        meta = list(viewport = "width=device-width, initial-scale=1.0"),
        script = "js/materialize.min.js", stylesheet = "css/materialize.min.css",
        head = "<link href=\"https://fonts.googleapis.com/icon?family=Material+Icons\" rel=\"stylesheet\">",
        attachment = NULL, package = NULL, all_files = TRUE)
    append(web$.tag, list(dep_materialise)) -> web$.tag
  }
  web$clear_setting <- function(){
    web$.tag <- list()
  }

  # browser()
  return(web)
}
#' Creat and view a webpage in http mode
#'
#' @param shinyTag default=NULL
#' @param js default=NULL, a file path to .js file, or a string of js scripts.
#' @param css default=NULL, a file path to .css file, or a string of css context.
#' @param ... other to pass to tagList
#'
#' @return
#' @export
#'
#' @examples none.
showTag <- function(shinyTag=NULL, js=NULL, css=NULL, ...){
  require(htmltools)
  quo_tag = rlang::enquo(shinyTag)
  tagname = rlang::as_label(quo_tag)
  dep_jquery =
    htmltools::htmlDependency(
      name = "jquery-mini",
      version = "3.6.0",
      src = list(
        href = "https://code.jquery.com"),
      meta = NULL,
      script = "jquery-3.6.0.min.js",
      stylesheet = NULL, head = NULL,
      attachment = NULL, package = NULL,
      all_files = TRUE)
  tag_js <- get_tag_js(js)
  tag_css <- get_tag_css(css)

  filepath <- create_filepath(
    filename=
      paste0(tagname,".html")
  )

  tagList(
    tags$div(shinyTag),
    tag_js,
    tag_css, ...,
    dep_jquery
  ) |>
    htmltools::save_html(filepath)
  show_page(filepath)
}
get_tag_js <- function(js) {
  if(is.null(js)){
    tag_js=NULL
  } else
    if(stringr::str_detect(js[[1]], ".js$")){
      htmltools::htmlDependency(
        name='temp',
        version="0",
        src=c(file=
            normalizePath(dirname(js[[1]]))),
        script=basename(js[[1]])
      ) -> tag_js
    } else {
      js=paste(js, collapse = "\n")
      tag_js = tags$script(
        js
      )
    }
  return(tag_js)
}

get_tag_css <- function(css) {
  if(is.null(css)){
    tag_css=NULL
  } else
    if(stringr::str_detect(css[[1]], ".css$")){
      htmltools::htmlDependency(
        name='temp_css',
        version="0",
        src=c(file=
            normalizePath(dirname(css[[1]]))),
        style=basename(css[[1]])
      ) -> tag_css
    } else {
      css=paste(collapse = "\n")
      tag_css = tags$style(
        css
      )
    }
  return(tag_css)
}
show_page <- function(filepath){
  servr::daemon_stop()
  servr::httd(normalizePath(
    dirname(filepath)),
    port=4321)
  browseURL(
    file.path(
      "http://127.0.0.1:4322",
      basename(filepath))
  )
}
create_filepath <- function(tag=NULL, destfolder=NULL, filename=NULL){
  if(is.null(destfolder)) destfolder="temp"
  if(!dir.exists(destfolder))
  {  message(
    glue::glue("Creating a folder '{destfolder}' to host the web page."))
    dir.create(destfolder)
  }
  if(is.null(filename)) filename="index.html"
  filepath=file.path(destfolder, filename)
}
