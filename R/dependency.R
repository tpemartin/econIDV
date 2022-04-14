footer_dependency = function(){
  htmltools::htmlDependency(
    name="footer",
    version="1.0.0",
    src=c(href="https://raw.githubusercontent.com/ntpuecon/econApp/main/assets/css/"),
    style="footer.css",
    all_files = F
  )}

Dependencies <- function(){
  dep=list()
  dep$plotly <-
    function(){
      htmltools::htmlDependency(
        name="plotly",
        version="2.9.0",
        src=c(href="https://cdn.plot.ly"),
        script = 'plotly-2.9.0.min.js'
      )
    }
  return(dep)
}

