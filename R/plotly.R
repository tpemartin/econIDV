#' Reorder legend groups based on desired group titles in order
#'
#' @param p a plotly plot with legend group titles already there.
#' @param legendgrouptitles a character vector of legend group titles in desired order
#'
#' @return a plotly plot
#' @export
#'
reorderTraceByLegendgrouptitles <- function(p, legendgrouptitles) {
  p |>
    econIDV::getTable_legendgrouptitle_trace() -> tbl_title_trace

  legendranks = 1:length(legendgrouptitles)
  names(legendranks) = legendgrouptitles
  for(.x in seq_along(tbl_title_trace$tracename)){
    p=plotly::style(
      p,
      legendrank=legendranks[[tbl_title_trace$grouptitle[[.x]]]],
      traces=tbl_title_trace$tracename[[.x]]
    )
  }
  p
}
#' Add legend group title to a plotly plot that already has legendgroup defined.
#'
#' @param p a plotly object.
#' @param legendgrouptitles default=NULL, or a vector of legendgrouptiles in desired order.
#'
#' @return a plotly object. If legendgrouptitles supplied, the plot will have legend groups presented in sequence as in legendgrouptitles.
#' @export
#'
add_legendgrouptitle <- function(p, legendgrouptitles=NULL) {
  getTable_legendgrouptitle_trace(p) -> groupMap2
  if(is.null(legendgrouptitles)){
    p |>
      style_legendgrouptitle(groupMap = groupMap2) -> p1
  } else {
    p |>
      style_legendgrouptitle2(groupMap = groupMap2,
        legendgrouptitles) -> p1
  }
  return(p1)
}
#' Get the table of legend group title and trace
#'
#' @param p a plotly object
#'
#' @return a data frame
#' @export
getTable_legendgrouptitle_trace <- function(p) {
  plotly::plotly_build(p) -> p_build
  seq_along(p_build$x$data) |>
    purrr::map_dfr(
      ~{
        data.frame(
          tracename=.x,
          grouptitle=p_build$x$data[[.x]]$legendgroup)}) -> groupMap

  groupMap |>
    dplyr::group_by(grouptitle) |>
    dplyr::summarise(
      tracename=min(tracename)
    ) |>
    dplyr::ungroup()
}
#' Import clipboard of Plotly Studio json view as a list
#'
#' @return
#' @export
#'
#' @examples none.
fromClipJSON <- function() {
  clipr::read_clip() -> xx
  stringr::str_subset(xx, "\\{[:digit:]+\\}$", negate=T) -> xx
  xx |>
    stringr::str_extract("^[^\t:]+") -> .patterns
  glue::glue("\\b{.patterns}\\b") -> .patterns2
  .replacements = paste0('"',.patterns,'"')
  names(.replacements)=.patterns2
  xx |> stringr::str_replace_all(.replacements) -> xx2
  xx2 |>
    stringr::str_remove_all("\t") -> xx3
  xx3 |>
    stringr::str_subset("(?<=:)[[:digit:]\\.]+$", negate=T) |>
    stringr::str_extract("(?<=:)[^:]+$")-> .patterns
  stringr::str_view(":#fff", "(?<=:)#fff$")
  .replacements = paste0('"',.patterns,'"')
  names(.replacements)=paste0("(?<=:)",.patterns, "$")
  xx3 |>
    stringr::str_replace_all(.replacements) -> xx4
  paste0('{',paste(xx4, collapse=","),"}") |>
    jsonlite::fromJSON() -> .json
  .json
}
#' Get ranked split names
#'
#' @param .plot a plotly plot
#'
#' @return
#' @export
#'
#' @examples none.
get_ranked_splitNames <- function(.plot) {
  splitNames=purrr::map_chr(
    seq_along(.plot$x$data),~.plot$x$data[[.x]]$name)
  splitNames
}

#' Add legend group titles based on ranked split names
#'
#' @param .plot a plotly plot
#' @param from a starting character number for extracting group titles from get_ranked_splitNames(.plot)
#' @param to a ending character number for extracting group titles from get_ranked_splitNames(.plot)
#'
#' @return
#' @export
#'
#' @examples none.
style_groupTitles_basedOnRankedSplitNames <- function(.plot, from, to) {
  .plot |>
    get_ranked_splitNames() -> ranked_splits

  groupNames = stringr::str_sub(ranked_splits,from,to)
  groupNames = factor(groupNames)
  groupNameLevels=levels(groupNames)
  purrr::map(
    seq_along(levels(groupNames)),
    ~{
      function(p){
        plotly::style(
          p,
          legendgrouptitle=list(text=groupNameLevels[[.x]]),
          traces=which(groupNames==groupNames[[.x]])
        )
      }
    }
  ) -> styleFns
  p=.plot
  for(.x in seq_along(styleFns)){
    p=styleFns[[.x]](p)
  }
  p
}
#' Create legend for diverging bins representing two parties.
#'
#' @param list_pal a named list of equal-length color vectors. Each vector consists of color codes. And the name of the vector represents the party name.
#' @param labels a character vector for labeling colors, should have the same length as any one color vector in `list_pal`
#' @param title title for the legend
#'
#' @return
#' @export
#'
#' @examples
#' colorspace::diverging_hcl(n = 10, h = c(245, 120), c = c(31, 100), l = c(30, 100), power = c(1, 1.3), register = "kmt-dpp") -> pal
#' list_pal = list(
#'   "國民黨"=rev(pal[1:5]),
#'   "民進黨"=pal[6:10]
#' )
#' labels=c("0-50","50-55","55-60","60-65","65-100%")
#'
#' legend_divergingBins(list_pal, labels, title=NULL)
legend_divergingBins <- function(list_pal, labels, title=NULL, width=200) {
  x=seq_along(list_pal[[1]])
  y=1:length(list_pal)
  expand.grid(x,y) |>
    setNames(c("x","y")) -> df
  df$fill=unlist(list_pal)
  groupnames=names(list_pal)
  asp=length(unique(df$y))/length(unique(df$x))*1.2
  ggplot(df, aes(x, y)) +
    geom_tile(aes(fill = I(fill)),width=0.9, height=0.9)+
    theme_classic()+
    theme(
      aspect.ratio = asp,
      axis.line=element_blank(),
      axis.ticks=element_blank(),
      axis.title = element_blank()
    )+
    scale_x_continuous(
      breaks=x,
      labels=labels
    )+
    scale_y_continuous(
      breaks=y,
      labels=groupnames
    ) -> gg
  plotly::ggplotly(gg, width=width,height=width*asp) |>
    plotly::layout(showlegend=F,
      title=list(
        text=title,
        font=list(size=10)
      ),
      xaxis=list(tickfont=list(size=6)),
      yaxis=list(tickfont=list(size=9))) |>
    plotly::config(
      # displayModeBar=F,
      # responsive=F,
      # scrollZoom = F,
      staticPlot=T
    )
}
#' Convert 6 digit hex color into a statement of "rgba(r, g, b, a)"
#'
#' @param hexcolor a character vector of 6 digits hex
#' @param alpha numeric, between 0 and 1. Default=1
#'
#' @return
#' @export
#'
#' @examples hex2rgba("#294968", "#217BBB")
hex2rgba <- function(hexcolor, alpha=1) {
  hexcolor |>
    colorspace::hex2RGB() -> rgbcolor
  rgbcolor@coords*255 |> round(digits = 0) -> rgb255color
  purrr::map_chr(1:nrow(rgb255color),
    ~{
      rgb255colorX = rgb255color[.x, ]
      glue::glue(
        'rgba({rgb255colorX[["R"]]}, {rgb255colorX[["G"]]},{rgb255colorX[["B"]]}, {alpha})')
    })
}


script_plotly <- function(plt0, id) {
  plt0 |>
    htmlwidgets:::createPayload() -> payload
  payload$x |> htmlwidgets:::toJSON() -> jsonX
  plotly_script(id, jsonX)
}

plotly_script <- function(id, jsonX){
  tags$script(
    glue::glue(
      "TESTER = document.getElementById('{id}');
  	Plotly.newPlot( TESTER, {jsonX} );"
    )) |>
    tagList(
      Dependencies()$plotly()
    )
}

find_li_with_attribute <- function(all_li, attr){
  purrr::map(
    all_li,
    ~{
      .x |>
        rvest::html_elements("a.attribute-name") |>
        rvest::html_text() }) -> list_attributenames
  purrr::map_lgl(
    list_attributenames,
    ~{
      any(stringr::str_detect(.x, glue::glue("\\b{attr}\\b")))
    }
  ) |> which() -> whichHasTargetAttribute
  all_li[whichHasTargetAttribute]
}
#' Get legend group titles in trace orders
#'
#' @param df a dataframe
#' @param label_by a name. The column of df that is used to generate legend label
#' @param group_by a name. The column of df that is used for legendgroup.
#'
#' @return a character vector of legendgrouptitles in trace order.
#'
#' @export
get_legendGroupTitles <- function(df, label_by, group_by){
  groupMap = get_groupMap(df, label_by, group_by)

  groupMap |>
    group_by(grouptitle) |>
    summarise(
      label=label[[1]],
      tracename=min(tracename)
    ) |>
    ungroup() |>
    arrange(tracename) -> groupMap2
  groupMap2

  groupMap2
}
get_groupMap <- function(df, label_by, group_by) {
  quo_label_by = rlang::enquo(label_by)
  quo_group_by = rlang::enquo(group_by)
  df |>
    dplyr::group_by(
      !!quo_label_by
    ) |>
    dplyr::slice(1) |>
    dplyr::select(
      !!quo_group_by, !!quo_label_by
    ) -> groupMap

  names(groupMap) <- c("grouptitle", "label")
  groupMap$tracename=1:nrow(groupMap)
  groupMap
}
style_legendgrouptitle <- function(p, groupMap) {
  for(.x in seq_along(groupMap$tracename)){
    p=plotly::style(
      p,
      legendgrouptitle=list(text=groupMap$grouptitle[[.x]]),
      traces=groupMap$tracename[[.x]]
    )
  }
  p
}

style_legendgrouptitle2 <- function(p, groupMap, legendgrouptitles) {
  legendranks = 1:length(legendgrouptitles)
  names(legendranks) = legendgrouptitles
  for(.x in seq_along(groupMap$tracename)){
    p=plotly::style(
      p,
      legendgrouptitle=list(text=groupMap$grouptitle[[.x]]),
      legendrank=legendranks[[groupMap$grouptitle[[.x]]]],
      traces=groupMap$tracename[[.x]]
    )
  }
  p
}
