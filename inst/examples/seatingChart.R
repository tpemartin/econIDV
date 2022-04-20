generate_seatingLayout = function(){
  set.seed(2938)
  seatingLayout = expand.grid(
    x=LETTERS[1:5],
    y=1:6
  ) 
  seatingLayout$names=randomNames::randomNames(nrow(seatingLayout))
  .GlobalEnv$seatingLayout <- seatingLayout
}
plot_seatingLayout = function(data){
  ggplot() +
    geom_point(
      data=data,
      aes(x=x,y=y), shape=0, size=15 
    ) -> gg0
  ggplotly(gg0) -> ggplt0
  ggplt0
}
get_json = function(){
  system.file("examples/seatingChart0.json", package="econIDV") |>
    jsonlite::fromJSON() -> pltJson 
  pltJson
}
plot_assignedSeats = function(data){
  ggplot() +
    geom_point(
      data=data,
      aes(x=x,y=y, text=names), color="red",
      size=6
    )-> gg1 
  ggplotly(gg1) -> ggplt1
  ggplt1
}
