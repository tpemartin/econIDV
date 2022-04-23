Setup=function(){
  setup=new.env()
  setup$from = function(...) {
    setup$.from = setup$.to =
      list(...)
    setup$to=function(...) {
      arglist=list(...)
      purrr::walk(
        names(arglist),
        ~{setup$.to[[.x]]=arglist[[.x]]}
      )
      df = dplyr::bind_rows(
        setup$.from |> as.data.frame(),
        setup$.to |> as.data.frame())
      rownames(df)=c("from", "to")

      setup$df = df
    }
  }
  return(setup)
}
get_slope_intercept <- function(setup){
  setup$df |>
    dplyr::mutate(
      slope=slope(px,py),
      intercept=intercept(I,py)
    ) -> setup$df
}
slope=function(px,py) -px/py
intercept=function(I,py) I/py
plot_budgetConstraints = function(df){
  require(ggplot2)
  ggplot(data=df)+
    geom_abline(
      aes(
        intercept=intercept,
        slope=slope,
        color=I(c("green","transparent")),
        frame=c("from","to")))
  # )+
  # scale_x_continuous(
  #   limits = c(0,20)
  # )+
  # scale_y_continuous(
  #   limits = c(0,100)
  # )
}
get_cobbDouglas_optim = function(setup){
  alpha = setup$.from$alpha
  beta = setup$.from$beta
  setup$df |>
    dplyr::mutate(
      cx=I*(alpha/(alpha+beta))/px,
      cy=I*(beta/(alpha+beta))/py
    ) ->
    setup$df
}
compute_utility= function(setup){
  setup$df |>
    dplyr::mutate(
      util=utility(cx,cy, setup)
    ) ->
    setup$df
}
utility=function(x,y,setup){
  alpha=setup$.from$alpha
  beta=setup$.from$beta
  x**alpha*y**beta
}
get_indifference_bundles0 <- function(setup) {
  setup$df["from",]$cx+c(0,seq(from=-2, to=3, length.out=100)) |> unique() |> sort() -> x

  x |> get_y_indifferenceCurve(
    u=setup$df['from',]$util,
    setup
  ) -> y
  df_indifferenceCurve =
    data.frame(
      x=x,
      y=y
    )
  return(df_indifferenceCurve)
}
get_indifference_bundles <- function(setup) {
  purrr::map(
    1:nrow(setup$df),
    ~{
      setup$df[.x,]$cx+c(0,seq(from=-2, to=3, length.out=100)) |> unique() |> sort() -> x
      data.frame(
        x=x,
        y=get_y_indifferenceCurve(
          x,
          u=setup$df[.x, ]$util,
          setup
        ))
    }
  ) -> list_indifferenceBundles

  setup$df$indifferenceBundles <- list_indifferenceBundles
}
get_y_indifferenceCurve = function(x, u, setup){
  alpha=setup$.from$alpha
  beta=setup$.from$beta
  y=(u/x**alpha)**(1/beta)
  return(y)
}
decompose_effect = function(setup){
  setup$df["Slutsky", ]=setup$df["to", ]
  setup$df["Slutsky", ]$I = (
    setup$df["to",]$px*setup$df["from", ]$cx+
      setup$df["to",]$py*setup$df["from",]$cy
  )
  get_slope_intercept(setup)
  get_cobbDouglas_optim(setup)
}
construct_decomposition = function(setup){
  get_cobbDouglas_optim(setup)
  compute_utility(setup)
  get_indifference_bundles(setup)
  decompose_effect(setup)
  setup$df$scenario=rownames(setup$df)
}
