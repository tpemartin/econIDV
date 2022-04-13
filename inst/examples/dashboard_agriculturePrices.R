plt_agPricesHighlight2 <- function(){
  require(plotly)
  require(plotly)
  require(ggplot2)

  df_agPrices |>
    highlight_key(~Item2) -> df_agPrices
  df_agPrices |>
    ggplot() -> base
  base +
    geom_line(
      mapping=aes(
        x=TIME_PERIOD,
        y=Item_VALUE,
        group=Item2
      )
    ) -> gg0
  ggplotly(gg0) |>
    highlight(
      on = "plotly_click",
      off = 'plotly_doubleclick'
    ) |>
    econIDV::showWidget()

}
plt_agPrices <- function(df){
  require(plotly)
  require(plotly)
  require(ggplot2)

  df |>
    ggplot() -> base
  base +
    geom_line(
      mapping=aes(
        x=TIME_PERIOD,
        y=Item_VALUE,
        group=Item2,
        text=paste0("<<",text, ">>")
      )) -> gg0
  ggplotly(gg0) -> ggplt0
  ggplt0$x$data[[1]]$text |>
    stringr::str_extract("(?<=<<)[^<>]+(?=>>)") -> ggplt0$x$data[[1]]$text

  ggplt0 |>
    plotly::style(
      hoverinfo="text"
    ) |>
  plotly::layout(
      xaxis=list(
        title=list(
          text=NULL # 不要axis name
        )
      ),
      yaxis=list(
        title=list(
          text=NULL
        )
      ),
      title=list(
        text="食物類消費者物價指數",
        pad=list(
          t=10
        ),
        yanchor="top"
      )
    )
}
plt_agInflation <- function(df){
  require(plotly)
  require(plotly)
  require(ggplot2)

  df |>
    ggplot() -> base
  base +
    geom_line(
      mapping=aes(
        x=TIME_PERIOD,
        y=growthRate,
        group=Item2
      )
    ) -> gg0
  ggplotly(gg0) |>
    plotly::layout(
      xaxis=list(
        title=list(
          text=NULL
        )
      ),
      yaxis=list(
        title=list(
          text=NULL
        )
      ),
      title=list(
        text="食物類消費者物價指數",
        pad=list(
          t=10
        ),
        yanchor="top"
      )
    )
}
dashboard_agPrices <- function(...){
  require(shinydashboard)
  dashboardPage(
    dashboardHeader(title="互動圖形"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("單純plotly", tabName = "plt0"),
        menuItem("Highlight", tabName="plt_highlight"),
        menuItem("Filter", tabName="plt_filter")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "plt0",
          box(plt0)
          ),
        tabItem(tabName = "plt_highlight",
          box_highlight[[1]],
          box_highlight[[2]],
          box_highlight[[3]],
          box_highlight[[4]]
          ),
        tabItem(tabName = "plt_filter",
          ...
          )

      )
    )
  )
}
save_plt_agPrices <- function() {
  if(!dir.exists("data")) dir.create("data")
  plt_agPrices() |>
    saveRDS("data/plt_agPricesLight.Rds")
  message("data/plt_agPricesLight.Rds is saved.")
}
load_plt_agPrices <- function(){
  .GlobalEnv$plt_agPrices = readRDS("data/plt_agPricesLight.Rds")
  message("Object plt_agPrices is loaded to the global environment.")
}
