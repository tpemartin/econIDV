#' Generate list of example environment
#'
#' @return
#' @export
#'
#' @examples none
Examples <- function(){
  list_ex <- list()
  list_ex$businessCycleLighting <-
    generate_example(
      Rscripts = c("examples/dashboard_businessCycleLighting.R",
        "examples/businessCycleLighting.R")
    )
  return(list_ex)
}

# -------------------------------------------------------------------------


generate_example <- function(Rscripts) {
  ex=new.env()
  ex$attach <- function() attach(ex)
  ex$detach <- function() detach(ex)
  Rscripts |>
    purrr::map(
      ~system.file(
        .x, package="econIDV"
      )
    ) -> RscriptPaths
  ex$Rscripts <- RscriptPaths
  RscriptPaths |>
    purrr::walk(
      source, local=ex
    )
  return(ex)
}
