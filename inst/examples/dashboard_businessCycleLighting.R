dash_businessCycleLighting <- function(){
  require(shiny)
  require(shinydashboard)
  dashboardPage(
    dashboardHeader(
      title="經濟數據儀表板"
    ),
    dashboardSidebar_custom(),
    dashboardBody_custom()
  ) -> tagUI
  tagUI |> econIDV::showWidget()
  invisible(tagUI) # return value only if there is a binding
}
dashboardSidebar_custom <- function(){
  dashboardSidebar(
    sidebarMenu(
      menuItem("景氣信號", tabName = "business_cycle", icon = shiny::icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = shiny::icon("th"))
    )
  )
}

dashboardBody_custom <- function(){
  dashboardBody(
    tabItems(
      tabItem(tabName = "business_cycle",
        fluidRow(
          valueBox(
            width=2,
            value=tags$img(
              src="lib/attachment-1/light_downturn.svg",
              style="display:block; margin-left:auto;margin-right:auto;", width="50%"
            ),
            subtitle=htmltools::HTML("<p style='text-align:center;'>本月信號</p>")
          ),
          column(
            width=2,
            econApp:::statisticCard(
              style="margin-top:0px;margin-bottom:15px;"
            )
          )
        ),
        fluidRow(
          box(plt_businessCycle(), width = 8)
        )
      ),
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
  )
}
