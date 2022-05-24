test_js <- function() {
  assertthat::assert_that(
    exists("session", envir=.GlobalEnv),
    msg="There is no session object in the global environment. Consider create one via econWeb::webdriverChromeSession()."
  )
  rstudioapi::getSourceEditorContext() -> document
  rstudioapi::documentSave(id=document$id)
  jsfile = document$path
  session$executeScript(
    htmltools::includeHTML(jsfile)
  )
}
