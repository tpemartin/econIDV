#' Start to serve web app at temp/ and lauch chromedriver for inspection
#'
#' @return a session
#' @export
apptest <- function(){
  serverJob()
  session = econWeb::webdriverChromeSession()
  webdriver::Session$new(
    port=9515)-> sessionx
  sessionx$go("http://127.0.0.1:4321/temp.html")
  return(sessionx)
}
#' chromedriver webdrive session starter
#'
#' @return an environment with $start_session() to start a webdriver session connected to chromedriver; with $kill_chrome() to kill chromedriver process.
#' @export
#'
webdriverChromeSession <- function() {
  sessionNew = new.env()
  # sessionNew$p <- processx::process$new("chromedriver", stdout="|")
  tryCatch({
    sessionNew$p <- processx::process$new("chromedriver", stdout = "|")
  },
    error=function(e){
      warning("Cannot locate chromedriver.")
      message("Make sure chromedriver is installed in one of Sys.getenv(\"PATH\") paths.")
    })

  sessionNew$kill_chrome = sessionNew$p$kill
  # sessionNew$p$read_output() -> stdout

  # browser()

  sessionNew$start_session = function(){
    webdriver::Session$new(
      port=get_port(
        sessionNew$p$read_output()
      )) -> sessionx
    allmethods = names(sessionx)
    purrr::walk(
      allmethods,
      ~assign(.x, sessionx[[.x]], envir = sessionNew
    ))
    #sessionx$showWidget = chromeDriverShowWidget(sessionx)
  }
  sessionNew}

get_port <- function(stdout) {
  stdout |>
    stringr::str_extract("(?<=(on\\sport\\s))[0-9]+") |> as.integer() -> port
  port
}
chromeDriverShowWidget <- function(sessionx){
  function(tag=.Last.value){
    if(!dir.exists("temp")) dir.create("temp")
    servr::daemon_stop()
    htmltools::save_html(
      htmltools::tagList(tag, dep_mobile()), file=file.path("temp","temp.html")
    )
    ss <- servr::httd("temp")
    # ss$port
    # rstudioapi::viewer(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
    sessionx$go(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
  }
}
