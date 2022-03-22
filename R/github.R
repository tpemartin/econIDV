#' prepare for github page publication
#'
#' @param tag a tag object
#'
#' @return
#' @export
#'
#' @examples none
prepare_githubPage <- function(tag) {
  dashboard0 <- tag
  htmltools::findDependencies(dashboard0) -> deps
  deps |> purrr::map_chr(~.x$name) -> depnames
  which(depnames=="AdminLTE") -> whichIsAdmin
  deps[[whichIsAdmin]] -> depx

  depx$src$file |> list.files() -> filesX
  depx$stylesheet |> stringr::str_which("^_") -> whichNeedsChange
  problemNames <- depx$stylesheet[whichNeedsChange]
  # .x=1
  for(.x in seq_along(problemNames)){
    correctname <- stringr::str_remove(problemNames[[.x]], "^_")
    problemfilepath = file.path(depx$src$file, problemNames[[.x]])
    correctfilepath = file.path(depx$src$file, correctname)
    if(file.exists(problemfilepath)) file.copy(
      from= problemfilepath ,
      to= correctfilepath,
      overwrite = T
    )
    depx$stylesheet[[whichNeedsChange[[.x]]]] <- correctname
  }
  deps[[whichIsAdmin]] <- depx

  htmltools::attachDependencies(dashboard0, deps, append = F) -> dashboard0
  htmltools::findDependencies(dashboard0) -> deps2
  htmltools::htmlDependencies(dashboard0) <- NULL
  htmltools::attachDependencies(dashboard0, deps, append=F) -> dashboard1
}
