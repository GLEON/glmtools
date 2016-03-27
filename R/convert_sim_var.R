
#' convert an existing simulation variable into a different one
#' 
#' allows you to provide specific variable or unit conversions to create a new variable 
#' that can be used with \code{\link{plot_var_compare}}, and others. 
#' 
#' @param nc_file the netcdf output file
#' @param name the name for the new variable
#' @param unit the units for the new variable
#' @param longname the longname for the new variable
#' @param \dots an expression to convert variable based on other variables or offsets
#' @export
#' @examples 
#' \dontrun{
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'convert_sim_var(nc_file, 'tempF','degF','temperature degrees Farenheit', temp/5*9+32)
#'plot_var(nc_file, 'tempF')
#' }
#' 
convert_sim_var <- function(nc_file='output.nc', name, unit, longname=name, ...){
  
  message('convert_sim_var is untested and in development')
  nc <- nc_open(nc_file, readunlim=TRUE, write=TRUE)
  lon <- nc$dim$lon
  lat <- nc$dim$lon
  z <- nc$dim$z
  time <- nc$dim$time
  missval = 9.96920996838687e36
  var_new <- ncvar_def(name, unit, dim = list(lon,lat,z,time), missval, prec="double")
  nc <- ncvar_add(nc, var_new)
  # // here, vals would be defined by the function passed in by `...`. Probably captured w/ lazyeval?
  # lazyeval::lazy_eval(convert, data=list(temp=ncvar_get(nc, 'temp')))
  convert <- lazyeval::lazy_dots(...)
  if (length(convert) > 1)
    stop('not yet ready to handle longer expressions')
  fun_string <- deparse(convert[[1]]$expr)
  variables <- strsplit(fun_string,'[19/*+-^]')[[1]]
  variables <- variables[variables != " " & variables != '']
  data <- lapply(variables, function(v) ncvar_get(nc, v))
  names(data) <- variables
  vals <- lazyeval::lazy_eval(convert, data=data)
  ncvar_put(nc, var_new, vals=vals[[1]])
  nc_close(nc)
}