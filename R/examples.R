#' Generate list of example environment
#'
#' @return
#' @export
#'
#' @examples none
Examples <- function(){
  list_ex <- new.env()
  list_ex$businessCycleLighting <- function(){
    generate_example(
      Rscripts = c("examples/dashboard_businessCycleLighting.R",
        "examples/businessCycleLighting.R")
    ) ->
    list_ex$businessCycleLightingEnv
  }

  list_ex$agricultureProductPrices <- function(){
    generate_example(
      Rscripts = c(
        "examples/agriculturePrices.R",
        "examples/dashboard_agriculturePrices.R"
      )
    ) ->
      list_ex$agricultureProductPricesEnv
  }
  list_ex$traffic_accident <- function(){
    generate_example(
      Rscripts = c(
        "examples/traffic_accident.R"
      ))->
        list_ex$traffic_accident_env
  }

  list_ex$seatingChart <- function(){
    generate_example(
      Rscripts = c(
        "examples/seatingChart.R"
      ))->
      list_ex$seatingChart_env
  }
  list_ex$income_substitution_effect <- function(){
    generate_example(
      Rscripts = c(
        "examples/income-substitution-effects.R"
      )
    ) -> list_ex$income_substitution_effect_env
  }

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
      source, local=ex, encoding="UTF-8"
    )
  return(ex)
}
