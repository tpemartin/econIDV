tag_sectionPanel_section <- function(title, imgUrl,imgWidget, story, storyTitle){
  # browser()
  tags$div(class = "sectionPanel-section",
    tags$div(class = "sectionPanel-section-sectionTitle",
      tags$div(class = "sectionPanel-section-sectionTitle-logo"),
      tags$div(class = "sectionPanel-section-sectionTitle-title",title)),
    tags$div(class = "sectionPanel-section-graph", style=glue::glue("background-image: url({imgUrl}) !important;"), imgWidget),
    tags$div(class = "sectionPanel-section-title",storyTitle),
    tags$div(class = "sectionPanel-section-story",story))
}
tag_sectionPanel <-function(title="經濟數據", imgUrl="/lib/img-1/graphdemo.png", imgWidget, storyTitle=NULL, story=NULL){
  if(!is.null(imgWidget)) imgUrl=""
  tags$div(class = "sectionPanel",
    tag_sectionPanel_section(title=title, imgUrl=imgUrl, imgWidget=imgWidget, storyTitle, story=story)
    )
}
sectionPanel_dependency <- function(){
  tagList(htmltools::htmlDependency(
    name="sectionPanel",
    version="1.0.0",
    src=c(file=normalizePath("./assets/css")),
    style="sectionPanel.css",
    all_files = F
  ),
  htmltools::htmlDependency(
    name="img",
    version=1,
    src=c(file=normalizePath("./assets/img")),
    attachment = "",
    all_files=T
  )
  )}
sectionPanel <- function(title="經濟數據", imgUrl="/lib/img-1/graphdemo.png", imgWidget=NULL, storyTitle=NULL, story=NULL, dependency=NULL){
  tagList(tag_sectionPanel(title=title, imgUrl=imgUrl, imgWidget=imgWidget, storyTitle=storyTitle, story=story), sectionPanel_dependency(), dependency)
}
# ui_sectionPanel() |> econWeb::browseTag2()
sectionPanel(story="Most data on the Russian home front are unreliable. The government has limited freedom of speech and arrested thousands of protesters. A few polls do show support for the war. In two surveys last month run by government-linked firms, around 65% of respondents backed the “special military operation”. Later independent polls found that 55-59% supported the military “action” or “operation”.

Such results must be taken with a cellar of salt, since the Kremlin has criminalised statements about the war that it deems false. But they still reveal political cleavages and trends over time. One poll found that being young or female, living in a big city, having a degree and not watching tv predicted anti-war views. Support may also be waning. In surveys of internet users in Moscow run by Alexei Navalny, an opposition leader, the share of people blaming Russia surged during the war’s first week.")
