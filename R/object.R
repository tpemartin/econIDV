#' Initiate an object with self saving method.
#'
#' @param objectname A symbol or a character
#'
#' @return
#' @export
#'
#' @examples Object(world)
Object <- function(objectname){
  chr_objname <- expr_objname <- rlang::enexpr(objectname)
  # browser()
  chr_objname <- rlang::expr_deparse(expr_objname)
  if(exists(chr_objname)){
    readline(message(chr_objname, " exists. Do you want to overwrite it? (yes/no)")) -> overwrite
    if(overwrite!="yes") return()
  }
  flag_chr <- stringr::str_detect(chr_objname,"\"")
  if(flag_chr) {
    chr_objname |> stringr::str_remove_all("\"") -> chr_objname
    expr_objname <- rlang::parse_expr(chr_objname)
  }

  rlang::expr(
    !!expr_objname <- list()
  ) -> declare_obj
  eval(declare_obj, envir = .GlobalEnv)
  value2assign <- new.env()
  value2assign$save=function(){
      filename = file.path("data",paste0(chr_objname,".Rds"))
      if(!dir.exists("data")) dir.create("data")
      saveRDS(.GlobalEnv[[chr_objname]], filename)
      message(filename," is saved.")
  }
  objload <- generate_objload(value2assign)

  value2assign$add <- function(obj){
    rlang::enexpr(obj) -> exprObj
    value2assign$objects[[
      rlang::expr_deparse(exprObj)
    ]] <- obj
    value2assign$load[[
      rlang::expr_deparse(exprObj)
    ]] <- function() objload(exprObj)
  }
  addObjExpr <- generate_addObjExpr(value2assign, objload)
  value2assign$addm <- function(...){
    rlang::enexprs(...) -> exprsObjs
    listObjs <- list(...)
    # browser()
    purrr::walk2(
      exprsObjs, listObjs, addObjExpr
    )
  }
  assign(chr_objname, value2assign, envir = .GlobalEnv)
}
generate_objload <- function(value2assign){
  function(exprObj){
    .GlobalEnv[[rlang::expr_deparse(exprObj)]] <-
      value2assign$objects[[rlang::expr_deparse(exprObj)]]
  }
}
generate_addObjExpr <- function(value2assign, objload){
  function(exprObj, obj){
    value2assign$objects[[
      rlang::expr_deparse(exprObj)
    ]] <- obj
    value2assign$load[[
      rlang::expr_deparse(exprObj)
    ]] <- function() objload(exprObj)
  }
}
