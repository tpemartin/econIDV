serverJob = function(){
  system.file("apptest/servr.R", package="econIDV") -> jobscript
  rstudioapi::jobRunScript(
    jobscript, workingDir=file.path(rprojroot::is_rstudio_project$make_fix_file()())
  )
}


