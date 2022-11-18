div_widget=function(plt)
electionDep = function(){
  htmltools::htmlDependency(
    name="election", version="1.0.0",
    src=c(file=normalizePath("test/")),
    stylesheet = "election.css"
  )
}
attachElectionDependencies = function(tag){
  htmltools::attachDependencies(
    tag, electionDep()
  ) |>
    attachMaterialiseDep()
}

elements = new.env()
elements$mapDiv = function(.plt=list_splitByCounties[[3]], cols="s5"){
  require(htmltools)
  div(
    class="row",
    div(
      class=glue::glue("col {cols}"),
      div_widget(.plt,
      elements$btn_float_action())
    )
  )
}
elements$btn_float_action = function(){
  actionMenu(title="大選年份", "2012", "2016", "2020") |>
    button_float_action()
}
elements$sidenaveTrigger = function(){
  button_float(icon("menu", class="grey-text"), class="right amber lighten-5") |> sidenavTrigger_boilerplate()
}

elements$sidenav = function(){
  trigger = elements$sidenaveTrigger()
  sidenav(
    tags$h5(class="center-align", "大選年份"),
    button("2020", class = "white black-text"),
    button("2016", class = "white black-text"),
    button("2012", class = "white black-text"),
    trigger=trigger)
}
elements$page=function(plt){
  tagList(
    tags$main(
      elements$mapDiv(.plt=plt, cols='s12 m5 offset-m3')
    )#,
    # footer(elements$btn_float_action())
  )
}

appstart <- function(){
  library(htmltools)
  devtools::load_all(".")
  customInteraction <<- readRDS("~/Github/interactive-data-visualization/bookdown/data/customInteraction.Rds")
  #customInteraction$load$list_splitByCounties()
  eval(rlang::expr(customInteraction$load$sf_electionsByYears()), envir = .GlobalEnv)
  eval(rlang::expr(customInteraction$load$label_share()), envir = .GlobalEnv)
  eval(rlang::expr(customInteraction$load$twoPartyPals()), envir = .GlobalEnv)

  plot_election() |>
    htmlwidgets::onRender("function(e){widget=e;}") -> plt
  plt |>
    elements$page() |>
    apptest::App() ->> app
  app$create()
  app$setup(2913)
  app$view()
  #
}

appupdate = function(){
  plot_election() |>
    htmlwidgets::onRender("function(e){widget=e;}") -> plt

  plt |>
    elements$page() |>
    app$update()
}

plot_election = function(){
  library(plotly)
  sf_electionsByYears[[3]] |>
    plotly::plot_ly(height=600) |>
    add_sf(
      legendgroup=~地區,
      name=~label_share(勝出政黨得票率),
      color=~勝黨得票率區間2,
      colors=twoPartyPals,
      alpha=1
    ) |>
    layout(showlegend=F,
      paper_bgcolor="transparent",
      plot_bgcolor="transparent") -> plt_election2020

  plt_election2020

}


