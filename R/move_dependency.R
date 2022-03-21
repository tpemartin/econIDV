#' Move dependency function out of R scripts to dependency.R in /R
#'
#' @param filepath R script filepath
#'
#' @return
#' @export
#'
#' @examples none
moveDep_out <- function(filepath) {
  xfun::read_utf8(filepath) -> lines
  rlang::parse_exprs(
    paste(lines, collapse = "\n")) -> exprs
  exprs |>
    purrr::map(~.x[[2]]) -> funnames
  funnames |> unlist() |> as.character() -> funnames

  where_is_funname <- generate_where_is_funname(lines)
  whichDep_start_end <- generate_whichDep_start_end(
    lines, funnames, where_is_funname
  )


  whichIsDependency <- which(funnames |> stringr::str_detect("dependency"))
  whichIsDependency |>
    purrr::map(
      whichDep_start_end
    ) -> list_dep_locs
  unlist(list_dep_locs) -> lines2remove
  if(length(lines2remove)==0) return(NA)
  deplines = lines[lines2remove]

  if(file.exists("R/dependency.R")){
    xfun::read_utf8("R/dependency.R") -> lines_dep
    lines_dep = c(lines_dep, deplines)
  } else {
    lines_dep=deplines
  }
  lines_dep |> xfun::write_utf8("R/dependency.R")
  lines[-lines2remove] |>
    xfun::write_utf8(con=filepath)
}
generate_where_is_funname <- function(lines){
  function(funname) {
    which(stringr::str_detect(
      lines, paste0("^",funname)
    ))
  }}
generate_whichDep_start_end <- function(lines, funnames, where_is_funname){
  function(whichIsDependency) {
    whichDepFunStarts = where_is_funname( funnames[[whichIsDependency]])

    if(whichIsDependency != length(funnames)){
      whichDepFunEnds = where_is_funname( funnames[[whichIsDependency+1]])-1
    } else {
      whichDepFunEnds=length(lines)
    }
    whichDepFunStarts:whichDepFunEnds
  }

}
moveAllDepsOut <- function(){
  list.files("./R", full.names = T) |> stringr::str_subset("dependency.R", T) -> all2scan
  all2scan |> normalizePath() -> all2scan
  moveDep_out_safely <- purrr::safely(econIDV::moveDep_out)
  all2scan |>
    purrr::walk(
      moveDep_out_safely
    )
}
