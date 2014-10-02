
.nml <- function(list_obj){
  nml <- list_obj
  class(nml) <- "nml"
  invisible(nml)
}