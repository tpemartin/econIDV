rvest::read_html("https://fonts.google.com/icons?selected=Material+Icons")-> .html
.html
.html |> 
  xml2::xml_child(2)

session = webdriverChromeSession()
session$start_session()
session$go("https://fonts.google.com/icons?selected=Material+Icons")
# need to scroll to the end before executing the following lines
el = session$executeScript('
  el = document.querySelector("#main-content > icons-page > div.aside-container.ng-star-inserted")
  return el.outerHTML;
  ')

el |> 
  rvest::read_html() -> .html
.html |> 
  rvest::html_elements("button > span.material-icons > span") -> icons
icons |>
  rvest::html_text() -> iconnames

iconnames |>
  jsonlite::toJSON() |>
  xfun::write_utf8("/Users/martinl/Github/econIDV/inst/material-design-icons/iconnames.json")

system.file("material-design-icons/iconnames.json", package="econIDV")
