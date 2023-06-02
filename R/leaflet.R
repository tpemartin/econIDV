
# <link rel="stylesheet" href="https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
# integrity="sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A=="
# crossorigin=""/>
leaflet_dependency <- function(){
  htmltools::htmlDependency(
    name="leaflet",
    version="1.7.1",
    src=c(href="https://unpkg.com/leaflet@1.7.1/dist/"),
    stylesheet = "leaflet.css",
    # list(
    #   src="leaflet.css",
    #   integrity="sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A==",
    #   crossorigin=""
    # ),
    script = list(
      src="leaflet.js",
      integrity="sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA==",
      crossorigin=""
    )
  )
}
example <- function(){
  require(htmltools)
  tagList(
    tags$div(id="map", style="height:300px;width:300px"),
    tags$script(HTML("
      var map = L.map('map').setView([51.505, -0.09], 13);
      L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}', {
      attribution: 'Map data &copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors, Imagery © <a href=\"https://www.mapbox.com/\">Mapbox</a>',
      maxZoom: 18,
      id: 'mapbox/streets-v11',
      tileSize: 512,
      zoomOffset: -1,
      accessToken: 'pk.eyJ1IjoidHBlbWFydGluIiwiYSI6ImNrbWVnaXRtcTB1cWsycGxqb2htbjA2emwifQ.ldwa2nW5syp4fN0UimXRLA'
      }).addTo(map);
    ")),
    leaflet_dependency()

  )


}


 #  <!-- Make sure you put this AFTER Leaflet's CSS -->
 # <script src="https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
 #   integrity="sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA=="
 #   crossorigin=""></script>
